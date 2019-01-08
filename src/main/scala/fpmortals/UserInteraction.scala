package fpmortals

import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.Url

final case class CodeToken(token: String, redirect_uri: String Refined Url)

trait UserInteraction[F[_]] {
  def start: F[String Refined Url]
  def open(uri: String Refined Url): F[Unit]
  def stop: F[CodeToken]
}