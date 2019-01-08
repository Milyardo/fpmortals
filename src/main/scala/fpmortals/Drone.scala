package fpmortals

import java.time.Instant
import java.time.format.DateTimeParseException

import scala.concurrent.duration._
import scalaz._
import Scalaz._
import contextual.Verifier

trait Drone[F[_]] {
  def getBacklog: F[Int]
  def getAgents: F[Int]
}

final case class MachineNode(id: String)

trait Machines[F[_]] {
  def getTime: F[Epoch]
  def getManaged: F[NonEmptyList[MachineNode]]
  def getAlive: F[Map[MachineNode, Epoch]]
  def start(node: MachineNode): F[MachineNode]
  def stop(node: MachineNode): F[MachineNode]
}

final case class Epoch(millis: Long) extends AnyVal {
  def +(d: FiniteDuration): Epoch = Epoch(millis + d.toMillis)
  def -(e: Epoch): FiniteDuration = (millis - e.millis).millis
}
object Epoch {
  object EpochInterpolator extends Verifier[Epoch] {
    def check(s: String): Either[(Int, String), Epoch] =
      try Right(Epoch(Instant.parse(s).toEpochMilli))
      catch {
        case _: DateTimeParseException => Left((0, "not in ISO-8601 format"))
      }
  }
}

final case class WorldView(
    backlog: Int,
    agents: Int,
    managed: NonEmptyList[MachineNode],
    alive: Map[MachineNode, Epoch],
    pending: Map[MachineNode, Epoch],
    time: Epoch
)

trait DynAgents[F[_]] {
  def initial: F[WorldView]
  def update(old: WorldView): F[WorldView]
  def act(world: WorldView): F[WorldView]
}

final class DynAgentsModule[F[_]: Applicative](D: Drone[F], M: Machines[F])
    extends DynAgents[F] {
  override def initial: F[WorldView] =
    ^^^^(D.getBacklog, D.getAgents, M.getManaged, M.getAlive, M.getTime) {
      case (db, da, mm, ma, mt) => WorldView(db, da, mm, ma, Map.empty, mt)
    }

  override def update(old: WorldView): F[WorldView] =
    initial.map({snap =>
      val changed = symdiff(old.alive.keySet, snap.alive.keySet)
      val pending = (old.pending -- changed).filterNot {
        case (_, started) => (snap.time - started) >= 10.minutes
      }
      snap.copy(pending = pending)
    })

  private def symdiff[T](a: Set[T], b: Set[T]): Set[T] =
    (a union b) -- (a intersect b)

  override def act(world: WorldView): F[WorldView] = world match {
    case NeedsAgent(node) =>
      M.start(node) >| world.copy(pending = Map(node -> world.time))
    case Stale(nodes) =>
      nodes.traverse { node =>
        M.stop(node) >| node
      }.map { stopped =>
        val updates = stopped.strengthR(world.time).toList.toMap
        world.copy(pending = world.pending ++ updates)
      }
    case _ => world.pure[F]
  }
}

private object NeedsAgent {
  def unapply(world: WorldView): Option[MachineNode] = world match {
    case WorldView(backlog, 0, managed, alive, pending, _)
        if backlog > 0 && alive.isEmpty && pending.isEmpty =>
      Option(managed.head)
    case _ => None
  }
}

private object Stale {
  def unapply(world: WorldView): Option[NonEmptyList[MachineNode]] =
    world match {
      case WorldView(backlog, _, _, alive, pending, time) if alive.nonEmpty =>
        (alive -- pending.keys)
          .collect {
            case (n, started)
                if backlog == 0 && (time - started).toMinutes % 60 >= 58 =>
              n
            case (n, started) if (time - started) >= 5.hours => n
          }
          .toList
          .toNel

      case _ => None
    }
}

final class Monitored[U[_]: Functor](program: DynAgents[U]) {
  type F[a] = Const[Set[MachineNode], a]
  private val D = new Drone[F] {
    def getBacklog: F[Int] = Const(Set.empty)
    def getAgents: F[Int]  = Const(Set.empty)
  }
  private val M = new Machines[F] {
    def getAlive: F[Map[MachineNode, Epoch]]     = Const(Set.empty)
    def getManaged: F[NonEmptyList[MachineNode]] = Const(Set.empty)
    def getTime: F[Epoch]                        = Const(Set.empty)
    def start(node: MachineNode): F[MachineNode]        = Const(Set.empty)
    def stop(node: MachineNode): F[MachineNode]         = Const(Set(node))
  }
  val monitor = new DynAgentsModule[F](D, M)

  def act(world: WorldView): U[(WorldView, Set[MachineNode])] = {
    val stopped = monitor.act(world).getConst
    program.act(world).strengthR(stopped)
  }
}