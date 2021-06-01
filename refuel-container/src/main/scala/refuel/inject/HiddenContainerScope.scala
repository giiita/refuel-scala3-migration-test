package refuel.inject

import refuel.container.DependencyPoolRef
import refuel.inject.Types.LocalizedContainer

private[refuel] class HiddenContainerScope[T](val fx: LocalizedContainer => T)(c: LocalizedContainer)
  extends DependencyPoolRef[LocalizedContainer] {
  it =>
  __refuel_cRef = Some(c)
}
