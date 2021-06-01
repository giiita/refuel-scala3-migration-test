package refuel.container

import refuel.container.Container
import refuel.inject.InjectionPriority
import refuel.container.InjectionPool.LazyConstruction
import refuel.container.provider.restriction.SymbolRestriction

import scala.quoted._

object InjectionPool {
  type DependencyConstructor[T] = Container => Seq[LazyConstruction[T]]
  type LazyConstruction[T]      = (InjectionPriority, Seq[InjectionPriority => SymbolRestriction[T]])
}

trait InjectionPool {

  /**
    * Get a list of injection-enabled declarations of any type.
    * Next to ModuleSymbol, reflect ClassSymbol.
    * A class / object with an effective annotation will be indexed into the container if it is an effective effect.
    *
    * @param wtt weak type tag.
    * @tparam T Type you are trying to get
    * @return
    */
  def collect[T](clazz: Class[_]): Container => Option[LazyConstruction[T]]
}