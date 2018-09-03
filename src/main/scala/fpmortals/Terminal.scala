package fpmortals

import scalaz._
import Scalaz._

import scala.concurrent.Future
import scala.io.StdIn
import scala.concurrent.ExecutionContext.Implicits.global

trait Terminal[C[_]] {
  def read: C[String]
  def write(in: String): C[Unit]
}

object IdTerminal extends Terminal[Id.Id] {
  override def read: Id[String] = StdIn.readLine()
  override def write(in: String): Id[Unit] = Console.println(in)
}

object FutureTerminal extends Terminal[Future] {
  override def read: Future[String] = Future(IdTerminal.read)
  override def write(in: String): Future[Unit] = Future(IdTerminal.write(in))
}

object IOTerminal extends Terminal[IO] {
  override def read: IO[String] = IO { IdTerminal.read }
  override def write(in: String): IO[Unit] = IO { IdTerminal.write(in) }
}