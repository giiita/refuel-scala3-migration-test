package refuel.inject

import refuel.container.{Container, ContainerImpl, ContainerLifecycle, InjectionPool}
import refuel.container.provider.Lazy

import scala.language.implicitConversions

given _containerInheritance[T]: scala.Conversion[HiddenContainerScope[T], T] = { x => x.fx(x.__refuel_c) }

given _explicitProviding[X](using x: Lazy[X]): scala.Conversion[Lazy[X], X] = new scala.Conversion[Lazy[X], X] {
  def apply(x: Lazy[X]): X = x._provide
}

//  given Container = ContainerImpl()
//  given InjectionPool = null

private[refuel] given ContainerLifecycleImpl: ContainerLifecycle with {
  private[this] lazy val _container: Container = ContainerImpl()
  private[this] lazy val _ip: InjectionPool = null

  def container: Container = _container
  def ip: InjectionPool = _ip
}

/**
 * Provide internal instance Lazy[T].
 * Once injected, the object is persisted.
 * However, if a request from a different container instance occurs, it may be searched again.
 *
 * @param variable Stored dependency object.
 * @tparam X Variable type
 * @return
 */
given _implicitProviding[X]: scala.Conversion[Lazy[X], X] = {
  _._provide
}

given _implicitNestedProviding[X]: scala.Conversion[Lazy[Lazy[X]], X] = {
  _._provide._provide
}
