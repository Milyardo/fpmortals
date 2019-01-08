package fpchallenge

import scalaz.Category

final case class Iso[A,B](f: A => B, g: B => A)
object Iso {
  implicit val isoCat: Category[Iso] = new Category[Iso] {
    override def id[A]: Iso[A, A] =
      Iso(identity, identity)
    override def compose[A, B, C](f: Iso[B, C], g: Iso[A, B]): Iso[A, C] =
      Iso(f.f compose g.f,f.g andThen g.g)
  }
}
