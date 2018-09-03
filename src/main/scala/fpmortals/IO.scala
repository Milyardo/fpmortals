package fpmortals

import scalaz.Monad

final class IO[A](val interpret: () => A) {
  def map[B](f: A => B): IO[B] = IO(f(interpret()))
  def flatMap[B](f: A => IO[B]): IO[B] = IO(f(interpret()).interpret())
}
object IO {
  def apply[A](a: => A): IO[A] = new IO(() => a)

  implicit val IOMonad: Monad[IO] = new Monad[IO] {
    override def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
    override def point[A](a: => A): IO[A] = IO(a)
    override def map[A, B](fa: IO[A])(f: A => B): IO[B] = fa.map(f)
  }
}
