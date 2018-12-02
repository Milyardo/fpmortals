package fpchallenge

import fpchallenge.Spiral2.TestCursor
import org.openjdk.jmh.annotations._
import scalaz.Scalaz._

@State(Scope.Benchmark)
class PureSpiralBench {
  @Param(Array("1", "2", "4", "6", "10", "50", "100", "1000"))
  var width: Int = _

  @Param(Array("1", "2", "4", "6", "10", "50", "100", "1000"))
  var height: Int = _

  @Benchmark
  def initialTaggedSpiral(): Unit = {
    Spiral.genSpiral(10,10)
  }

  @Benchmark
  def finalTaglessSpiral(): Unit = {
    Spiral2.spiral(TestCursor)(10,10)
  }
}
