package fpchallenge

import scalaz._
import Scalaz._
import fpmortals.IO

final case class Log[L, A](
    debug: L => A,
    info : L => A,
    error: L => A
)
object Log {

  def unlevelledLog[L, A](f: L => A): Log[L, A] = Log(f, f, f)

  val levelledLog = Log[String, String](
    debug = msg => s"Debug: $msg",
    info = msg => s"Info: $msg",
    error = msg => s"error: $msg",
  )

  val consoleLog: Log[String, IO[Unit]] = unlevelledLog(msg => IO(println(msg)))

  val logWithPlace: Log[(String, String), String] = unlevelledLog({
    case (place, msg) => s"[$place]: $msg"
  })

  def defaultLogger: Log[(String, String), IO[Unit]] =
    (levelledLog <<< logWithPlace) >>> consoleLog

  implicit val logProFunctor: Profunctor[Log] = new Profunctor[Log] {
    override def mapfst[A, B, C](fab: Log[A, B])(f: C => A): Log[C, B] = Log(
      debug = fab.debug compose f,
      info = fab.info compose f,
      error = fab.error compose f
    )

    override def mapsnd[A, B, C](fab: Log[A, B])(f: B => C): Log[A, C] = Log(
      debug = fab.debug andThen f,
      info = fab.info andThen f,
      error = fab.error andThen f,
    )
  }

  implicit val logCategory: Category[Log] = {
    new Category[Log] {
      override def id[A]: Log[A, A] = Log(a => a, a => a, a => a)
      override def compose[A, B, C](f: Log[B, C], g: Log[A, B]): Log[A, C] =
        Log(
          debug = f.debug compose g.debug,
          info = f.info compose g.info,
          error = f.error compose g.error
        )
    }
  }
}
