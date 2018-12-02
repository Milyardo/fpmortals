package fpchallenge

import scalaz._
import Scalaz._
import Command._
import Direction._
import fpmortals.IO

object Spiral2 {

  trait Cursor[F[_]] {
    def put(n: Int): F[Unit]
    def move(dir: Direction): F[Unit]
  }

  def spiral[F[_]: Monad](C: Cursor[F])(
      width: Int,
      height: Int): F[Unit] = {
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
                prev: F[Unit],
                currentDir: Direction,
                depth: Int,
                right: Int,
                bottom: Int,
                left: Int,
                top: Int): F[Unit] = {
      if (depth > width * height) {
        prev
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
        val prog = for {
          _ <- prev
          _ <- C.put(depth)
          _ <- C.move(next)
        } yield ()

        recurse(x2, y2, prog, next, depth + 1, r2, b2, l2, t2)
      }
    }
    recurse(1, 1, ().point[F], Right, 1, width, height, 1, 2)
  }

  class IOCursor private (startX: Int, startY: Int) extends Cursor[IO] {

    var x: Int = startX
    var y: Int = startY

    override def put(n: Int): IO[Unit] = IO(printf("%-2d", n))
    override def move(dir: Direction): IO[Unit] = dir match {
      case Left =>
        IO {
          x = x - 3
          print(s"\u001B[$y;${x}H")
        }
      case Right =>
        IO {
          x = x + 3
          print(s"\u001B[$y;${x}H")
        }
      case Up =>
        IO {
          y = y - 1
          print(s"\u001B[$y;${x}H")
        }
      case Down =>
        IO {
          y = y + 1
          print(s"\u001B[$y;${x}H")
        }
    }
  }
  object IOCursor {
    def apply(startX: Int, startY: Int): IO[IOCursor] =
      IO(new IOCursor(startX, startY))
  }

  def delay(cursor: Cursor[IO], ms: Int): Cursor[IO] = new Cursor[IO] {
    override def put(n: Int): IO[Unit] = cursor.put(n)
    override def move(dir: Direction): IO[Unit] =
      for {
        _ <- cursor.move(dir)
        _ <- IO(Thread.sleep(ms))
      } yield ()
  }

  object TestCursor extends Cursor[Writer[List[Command], ?]] {
    override def put(n: Int): Writer[List[Command], Unit] =
      Writer(List(Put(n)), ())

    override def move(dir: Direction): Writer[List[Command], Unit] =
      Writer(List(Move(dir)), ())
  }

  def main(args: Array[String]): Unit = {
    val width = 10
    val height = 10
    val prog = for {
      _      <- IO(print(ClearScreen))
      _      <- IO(print(s"\u001B[1;1H"))
      cursor <- IOCursor(1, 1)
      _      <- spiral(cursor)(width, height)
      _      <- IO(print(s"\u001B[${height + 1};1H"))
      prog2  = spiral(TestCursor)(width, height)
      _      <- IO(println(prog2.run._1))
    } yield ()
    prog.interpret()
  }

  final val ClearScreen = "\u001B[2J"
}
