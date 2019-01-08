package fpmortals

import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.Url

final case class ServerConfig(
    auth: String Refined Url,
    access: String Refined Url,
    refresh: String Refined Url,
    scope: String,
    clientId: String,
    clientSecret: String
)
final case class RefreshToken(token: String)
final case class BearerToken(token: String, expires: Epoch)
