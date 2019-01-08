package fpmortals

trait LocalClock[F[_]] {
  def now: F[Epoch]
}