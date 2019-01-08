package fpchallenge

import scalaz.Id.Id

trait FunctorC[F[_],G[_], ->>[_,_], ->>>[_,_]] {
  def mapC[A,B](f: F[A] ->> F[B]): G[A] ->>> G[B]
}
trait EFunctor[F[_]] extends FunctorC[Id,F,Function,Function] {
  final def map[A,B](as: F[A])(f: A => B): F[B] =
    mapC[A,B](f)(as)
}

object ListFunctor extends EFunctor[List] {
  override def mapC[A, B](f: A => B): List[A] => List[B] = {
    case Nil => Nil
    case head :: tail => f(head) :: mapC(f)(tail)
  }
}