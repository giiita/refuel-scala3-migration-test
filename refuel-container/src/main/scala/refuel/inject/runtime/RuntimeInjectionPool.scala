package refuel.inject.runtime

import refuel.container.Container
import refuel.container.InjectionPool
import refuel.container.InjectionPool.LazyConstruction

object RuntimeInjectionPool extends InjectionPool {
  def collect[T](clazz: Class[_]): Container => Option[LazyConstruction[T]] = { c => None }
}
