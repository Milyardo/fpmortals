package fpchallenge

import scalaz._, Scalaz._

final case class Crud[PK, E](
    read: PK => Option[E],
    create: E => PK,
    update: (PK, E) => Unit,
    delete: PK => Unit
)
object Crud {
  implicit val crudCategory: Category[Crud] = new Category[Crud] {
    override def id[A]: Crud[A, A] = Crud(
      read   = a => Some(a),
      create = a => a,
      update = (a, a2) => (),
      delete = a => ()
    )
    override def compose[A, B, C](f: Crud[B, C], g: Crud[A, B]): Crud[A, C] =
      Crud(
        read = Kleisli(g.read).andThenK(f.read).run,
        create = f.create andThen g.create,
        update = (a, c) => {
          f.update(null,c) //(B,C) => Unit???
          g.update(a,null) //(A,B) => Unit???
        },
        delete = g.delete //f: B => Unit ???

      )
  }
}
