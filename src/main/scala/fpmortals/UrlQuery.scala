package fpmortals

import java.net.URLEncoder

import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.Url
import scalaz.IList
import scalaz.std.string._
import scalaz.syntax.foldable._
import simulacrum.typeclass

// URL query key=value pairs, in un-encoded form.
final case class UrlQuery(params: List[(String, String)])

@typeclass trait UrlQueryWriter[A] {
  def toUrlQuery(a: A): UrlQuery
}

@typeclass trait UrlEncodedWriter[A] {
  def toUrlEncoded(a: A): String Refined UrlEncoded
}
object UrlEncodedWriter {

  implicit val encoded: UrlEncodedWriter[String Refined UrlEncoded] = s => s

  implicit val url: UrlEncodedWriter[String Refined Url] = s => Refined.unsafeApply(s.value)

  implicit val string: UrlEncodedWriter[String] =
    s => Refined.unsafeApply(URLEncoder.encode(s, "UTF-8"))

  implicit val long: UrlEncodedWriter[Long] =
    s => Refined.unsafeApply(s.toString)

  implicit def ilist[K: UrlEncodedWriter, V: UrlEncodedWriter]
    : UrlEncodedWriter[IList[(K, V)]] = { m =>
    import ops._
    val raw = m
      .map({
        case (k, v) => k.toUrlEncoded.value + "=" + v.toUrlEncoded.value
      })
      .intercalate("&")
    Refined.unsafeApply(raw) // by deduction
  }

}
