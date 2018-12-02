package fpchallenge

/*
A program that puts 1 to x*y in a two dimensional array in a spiral pattern,
moving from the top left to the center clockwise
as such:
1  2  3  4  5  6
14 15 16 17 18 7
13 12 11 10 9  8
 */
object Spiral {


  import Command._
  import Direction._

  def genSpiral(width: Int,
                height: Int): List[Command] = {
    def isEdge(x: Int,
               y: Int,
               r: Int,
               bottom: Int,
               l: Int,
               top: Int,
               dir: Direction): Boolean =
      dir match {
        case Left  => x == l
        case Right => x == r
        case Up    => y == top
        case Down  => y == bottom
      }
    def move(x: Int, y: Int, dir: Direction): (Int, Int) = dir match {
      case Left  => (x - 1, y)
      case Right => (x + 1, y)
      case Up    => (x, y - 1)
      case Down  => (x, y + 1)
    }

    def recurse(x: Int,
                y: Int,
                tail: List[Command],
                currentDir: Direction,
                depth: Int,
                right: Int,
                bottom: Int,
                left: Int,
                top: Int): List[Command] = {
      if (depth > width * height) {
        tail
      } else {
        val (next, r2, b2, l2, t2) =
          if (isEdge(x, y, right, bottom, left, top, currentDir)) {
            val n = currentDir.cw
            val (rr, bb, ll, tt) = currentDir match {
              case Left  => (right, bottom, left + 1, top)
              case Down  => (right, bottom - 1, left, top)
              case Right => (right - 1, bottom, left, top)
              case Up    => (right, bottom, left, top + 1)
            }
            (n, rr, bb, ll, tt)
          } else {
            (currentDir, right, bottom, left, top)
          }
        val (x2, y2) = move(x, y, next)
        recurse(x2,
          y2,
          Move(next) :: Put(depth) :: tail,
          next,
          depth + 1,
          r2,
          b2,
          l2,
          t2)
      }
    }

    recurse(1, 1, List.empty, Right, 1, width, height, 1, 2).reverse
  }

  def printSpiral(x: Int, y: Int, prog: List[Command]): Unit = {
    def recurse(x: Int, y: Int, prog: List[Command]): Unit = prog match {
      case Nil => ()
      case op :: tail =>
        val (x2, y2) = op match {
          case Put(n) =>
            printf("%-2d", n)
            (x, y)
          case Move(Left) =>
            val xx = x - 3
            print(s"\u001B[$y;${xx}H")
            (xx, y)
          case Move(Right) =>
            val xx = x + 3
            print(s"\u001B[$y;${xx}H")
            (xx, y)
          case Move(Up) =>
            val yy = y - 1
            print(s"\u001B[$yy;${x}H")
            (x, yy)
          case Move(Down) =>
            val yy = y + 1
            print(s"\u001B[$yy;${x}H")
            (x, yy)
        }
        recurse(x2, y2, tail)
    }
    print(s"\u001B[$y;${x}H")
    recurse(x, y, prog)
    print(s"\u001B[${prog.length};${x}H")
  }

  def main(args: Array[String]): Unit = {
    val prog = genSpiral(3, 6)
    print(ClearScreen)
    printSpiral(1, 1, prog)
    println(prog)
  }
  final val ClearScreen = "\u001B[2J"
}


sealed trait Command
object Command {
  case class Put(c: Int) extends Command
  case class Move(d: Direction) extends Command
}
sealed trait Direction {
  def cw: Direction
  def ccw: Direction
}
object Direction {
  case object Left extends Direction {
    override def cw: Direction = Up
    override def ccw: Direction = Down
  }
  case object Right extends Direction {
    override def cw: Direction = Down
    override def ccw: Direction = Up
  }
  case object Up extends Direction {
    override def cw: Direction = Right
    override def ccw: Direction = Left
  }
  case object Down extends Direction {
    override def cw: Direction = Left
    override def ccw: Direction = Right
  }
}
