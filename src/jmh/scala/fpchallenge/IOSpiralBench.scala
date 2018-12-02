package fpchallenge

import fpchallenge.Spiral2.IOCursor
import org.openjdk.jmh.annotations._


@State(Scope.Benchmark)
class IOSpiralBench {
  @Param(Array("1", "2", "4", "6", "10"))
  var width: Int = _

  @Param(Array("1", "2", "4", "6", "10"))
  var height: Int = _

  @Benchmark
  def initialTaggedSpiral(): Unit = {
    Spiral.printSpiral(1,1,Spiral.genSpiral(width,height))
  }

  @Benchmark
  def finalTaglessSpiral(): Unit = {
    for {
      cursor <- IOCursor(1,1)
      _      <- Spiral2.spiral(cursor)(width,height)
    } yield ()
  }
}
