package fpmortals

import org.scalatest.{FlatSpec, Matchers}
import Data._

class DynAgentBusinessTest extends FlatSpec with Matchers {
  "Business Logic" should "generate an initial world view" in {
    val mutable = new Mutable(needsAgents)
    import mutable._

    program.initial shouldBe needsAgents
  }

  it should "remove changed nodes from pending" in {
    val world = WorldView(0, 0, managed, Map(node1 -> time3), Map.empty, time3)
    val mutable = new Mutable(world)
    import mutable._

    val old =
      world.copy(alive = Map.empty, pending = Map(node1 -> time2), time = time2)
    program.update(old) shouldBe world
  }

  it should "request agents when needed" in {
    val mutable = new Mutable(needsAgents)
    import mutable._

    val expected = needsAgents.copy(
      pending = Map(node1 -> time1)
    )

    program.act(needsAgents) shouldBe expected

    mutable.stopped shouldBe 0
    mutable.started shouldBe 1
  }

  it should "not request agents when pending" in {
    val doesNotNeedAgents = needsAgents.copy(pending = Map(node1 -> time1))

    val mutable = new Mutable(doesNotNeedAgents)
    import mutable._

    program.act(doesNotNeedAgents) shouldBe doesNotNeedAgents

    mutable.stopped shouldBe 0
    mutable.started shouldBe 0
  }

  it should "don’t shut down agents if nodes are too young" in {
    val hasYoungAgents =
      needsAgents.copy(backlog = 0, alive = Map(node1 -> time1), time = time2)
    val mutable = new Mutable(hasYoungAgents)
    import mutable._

    program.act(hasYoungAgents) shouldBe hasYoungAgents
    mutable.stopped shouldBe 0
  }
  it should "shut down agents when there is no backlog and nodes will shortly incur new costs" in {
    val hasOldAgents =
      needsAgents.copy(backlog = 0, alive = Map(node1 -> time1), time = time3)
    val mutable = new Mutable(hasOldAgents)
    import mutable._

    program.act(hasOldAgents) shouldBe hasOldAgents.copy(
      pending = Map(node1 -> time3))
    mutable.stopped shouldBe 1
  }

  it should "shut down agents, even if they are potentially doing work, if they are too old" in {
    val hasOldAgents =
      needsAgents.copy(backlog = 0, alive = Map(node1 -> time1), time = time4)
    val mutable = new Mutable(hasOldAgents)
    import mutable._

    program.act(hasOldAgents)
    mutable.stopped shouldBe 1
    mutable.started shouldBe 0
  }

  it should "ignore unresponsive pending actions during update" in {
    val hasUnresponsiveActions =
      needsAgents.copy(backlog = 0, pending = Map(node1 -> time4), time = time1)
    val mutable = new Mutable(hasUnresponsiveActions)
    import mutable._

    program.update(hasUnresponsiveActions)
    mutable.stopped shouldBe 0
    mutable.started shouldBe 0
  }
}
