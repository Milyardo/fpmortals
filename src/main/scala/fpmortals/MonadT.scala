package fpmortals

import scalaz._, Scalaz._

object MonadT {
  def emails: List[Option[String]] =
    List(Some("123@fakemail.com"),
         None,
         Some("dev@null.net"),
         None,
         Some("nigerian.prince@spam.gov"))

  def ages: List[Option[Int]] = List(
    Some(23),
    None,
    None,
    Some(5),
    Some(15)
  )

  def idNumbers: List[Long] = List(
    123L,
    1337L,
    1024L,
    1080L,
    -9000L
  )

  def users: OptionT[List, (Long, String, Int)] = for {
    e <- OptionT(emails)
    n <- OptionT(ages)
    id <- liftM(idNumbers)
  } yield (id,e,n)

  def liftM[F[_]: Functor, A](fa: F[A]): OptionT[F,A] = OptionT(fa.map(a => Some(a)))

  def main(args: Array[String]): Unit = {
    println(users.run)
  }
}
