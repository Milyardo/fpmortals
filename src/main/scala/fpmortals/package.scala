import contextual.Prefix
import fpmortals.Epoch.EpochInterpolator
package object fpmortals {
  implicit class EpochMillisStringContext(sc: StringContext) {
    val epoch = Prefix(EpochInterpolator, sc)
  }
}
