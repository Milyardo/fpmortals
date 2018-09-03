package fpmortals

import scalaz._
import Scalaz._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


object MyApp {

  def echo[C[_]: Monad](term: Terminal[C]): C[String] =
    for {
      in <- term.read
      _ <- term.write(in)
    } yield in

  def prompt[C[_]: Monad](term: Terminal[C])(message: String)(response: String => String): C[String] =
    for {
      _ <- term.write(message)
      in <- echo(term)
      _ <- term.write(response(in))
    } yield in

  def main(args: Array[String]): Unit = {
    val result1: IO[String] = echo(IOTerminal)
    val result2: IO[String] = echo(IOTerminal)

    val prompt1 = prompt(IOTerminal)("What is your name")(name => s"Hello $name")
    val prompt2 = prompt(IOTerminal)("How old are you?")(age => "That's fairly old.")

    println(s"result2: ${prompt2.interpret()}")
  }
}
