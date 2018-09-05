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

final class DynAgentsModule[F[_]: Monad](D: Drone[F], M: Machines[F])
    extends DynAgents[F] {
  override def initial: F[WorldView] =
    ^^^^(D.getBacklog, D.getAgents, M.getManaged, M.getAlive, M.getTime) {
      case (db, da, mm, ma, mt) => WorldView(db, da, mm, ma, Map.empty, mt)
    }

  override def update(old: WorldView): F[WorldView] =
    for {
      snap <- initial
      changed = symdiff(old.alive.keySet, snap.alive.keySet)
      pending = (old.pending -- changed).filterNot {
        case (_, started) => (snap.time - started) >= 10.minutes
      }
      update = snap.copy(pending = pending)
    } yield update

  private def symdiff[T](a: Set[T], b: Set[T]): Set[T] =
    (a union b) -- (a intersect b)

  override def act(world: WorldView): F[WorldView] = world match {
    case NeedsAgent(node) =>
      for {
        _ <- M.start(node)
        update = world.copy(pending = Map(node -> world.time))
      } yield update

    case Stale(nodes) =>
      for {
        stopped <- nodes.traverse(M.stop)
        updates = stopped.map(_ -> world.time).toList.toMap
        update = world.copy(pending = world.pending ++ updates)
      } yield update

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
