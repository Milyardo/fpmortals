package fpmortals

import scalaz._, Scalaz._
import contextual._
import java.time.Instant

object Data {
  val node1 = MachineNode("1243d1af-828f-4ba3-9fc0-a19d86852b5a")
  val node2 = MachineNode("550c4943-229e-47b0-b6be-3d686c5f013f")
  val managed = NonEmptyList(node1, node2)

  val time1: Epoch = epoch"2017-03-03T18:07:00Z"
  val time2: Epoch = epoch"2017-03-03T18:59:00Z" // +52 mins
  val time3: Epoch = epoch"2017-03-03T19:06:00Z" // +59 mins
  val time4: Epoch = epoch"2017-03-03T23:07:00Z" // +5 hours

  val needsAgents = WorldView(5, 0, managed, Map.empty, Map.empty, time1)
}

class Mutable(state: WorldView) {
  var started, stopped: Int = 0

  private val D: Drone[Id] = new Drone[Id] {
    def getBacklog: Int = state.backlog
    def getAgents: Int = state.agents
  }

  private val M: Machines[Id] = new Machines[Id] {
    def getAlive: Map[MachineNode, Epoch] = state.alive
    def getManaged: NonEmptyList[MachineNode] = state.managed
    def getTime: Epoch = state.time
    def start(node: MachineNode): MachineNode = { started += 1; node }
    def stop(node: MachineNode): MachineNode = { stopped += 1; node }
  }

  val program = new DynAgentsModule[Id](D, M)
}

final case class World(
    backlog: Int,
    agents: Int,
    managed: NonEmptyList[MachineNode],
    alive: Map[MachineNode, Epoch],
    started: Set[MachineNode],
    stopped: Set[MachineNode],
    time: Epoch
)

import State.{ get, modify }
object StateImpl {
  type F[a] = State[World, a]

  private val D = new Drone[F] {
    def getBacklog: F[Int] = get.map(_.backlog)
    def getAgents: F[Int]  = get.map(_.agents)
  }

  private val M = new Machines[F] {
    def getAlive: F[Map[MachineNode, Epoch]]   = get.map(_.alive)
    def getManaged: F[NonEmptyList[MachineNode]] = get.map(_.managed)
    def getTime: F[Epoch]                      = get.map(_.time)

    def start(node: MachineNode): F[MachineNode] =
      modify[World](w => w.copy(started = w.started + node)).map(_ => node)
    def stop(node: MachineNode): F[MachineNode] =
      modify[World](w => w.copy(stopped = w.stopped + node)).map(_ => node)
  }

  val program = new DynAgentsModule[F](D, M)
}