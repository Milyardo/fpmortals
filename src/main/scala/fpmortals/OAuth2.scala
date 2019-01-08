package fpmortals

import eu.timepit.refined
import refined.api.Refined
import refined.string.Url

import UrlEncodedWriter.ops._

final case class AuthRequest(
    redirect_uri: String Refined Url,
    scope: String,
    client_id: String,
    prompt: String = "consent",
    response_type: String = "code",
    access_type: String = "offline"
)
object AuthRequest {
  implicit val query: UrlQueryWriter[AuthRequest] = { a =>
    UrlQuery(List(
      "redirect_uri"  -> a.redirect_uri.value,
      "scope"         -> a.scope,
      "client_id"     -> a.client_id,
      "prompt"        -> a.prompt,
      "response_type" -> a.response_type,
      "access_type"   -> a.access_type
    ))
  }
}

final case class AccessRequest(
    code: String,
    redirect_uri: String Refined Url,
    client_id: String,
    client_secret: String,
    scope: String = "",
    grant_type: String = "authorization_code"
)
object AccessRequest {
  implicit val encoded: UrlEncodedWriter[AccessRequest] = { a =>
    List(
      "code"          -> a.code.toUrlEncoded,
      "redirect_uri"  -> a.redirect_uri.toUrlEncoded,
      "client_id"     -> a.client_id.toUrlEncoded,
      "client_secret" -> a.client_secret.toUrlEncoded,
      "scope"         -> a.scope.toUrlEncoded,
      "grant_type"    -> a.grant_type.toUrlEncoded
    ).toUrlEncoded
  }
}

final case class AccessResponse(
    access_token: String,
    token_type: String,
    expires_in: Long,
    refresh_token: String
)

final case class RefreshRequest(
    client_secret: String,
    refresh_token: String,
    client_id: String,
    grant_type: String = "refresh_token"
)
object RefreshRequest {
  implicit val encoded: UrlEncodedWriter[RefreshRequest] = { r =>
    List(
      "client_secret" -> r.client_secret.toUrlEncoded,
      "refresh_token" -> r.refresh_token.toUrlEncoded,
      "client_id"     -> r.client_id.toUrlEncoded,
      "grant_type"    -> r.grant_type.toUrlEncoded
    ).toUrlEncoded
  }
}

final case class RefreshResponse(
    access_token: String,
    token_type: String,
    expires_in: Long
)