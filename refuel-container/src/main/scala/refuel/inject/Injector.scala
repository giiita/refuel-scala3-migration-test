package refuel.inject

import refuel.container.`macro`.Macro
import refuel.container.{Container, ContainerImpl, ContainerLifecycle, DependencyPoolRef, IndexedKey, Indexer, InjectionPool}
import refuel.container.provider.Accessor
import refuel.inject.Types.LocalizedContainer
import refuel.container.provider.{Accessor, Lazy}

import scala.quoted.Type

trait Injector extends DependencyPoolRef[Container] { me =>
  /* Container instance */
  this.__refuel_cRef = Some(summon[ContainerLifecycle].container)
  /**
   * This refers to itself.
   * When injecting, check if this Accessor is authorized.
   *
   * @return
   */
  protected given __refuel_accessor: Accessor[_] = Accessor(me)

  /**
   * Implicitly injection pool
   *
   * @return
   */
  protected given __refuel_ip: InjectionPool = null

  /**
   * Manually register the new dependency.
   *
   * @param x        new dependency.
   * @param priority Injection priority.
   * @tparam T new dependency type
   */
//  @deprecated("Try (t).index() instead.")
//  def overwrite[T: Type](x: T, priority: InjectionPriority = InjectionPriority.Overwrite): Unit = __refuel_cRef.createIndexer(IndexedKey.from[T], x, priority, Vector.empty).indexing()
//
//  /**
//   * Create a container shade.
//   * You can't nest shading.
//   *
//   * {{{
//   *   class A(value: String)
//   *   class B(a: A)
//   *   class C(b: B)
//   *   class D(c: C)
//   *   shade { implicit c =>
//   *     new BStub().index[B]()
//   *     inject[D].run // Use BStub in C in D
//   *   }
//   *   inject[D].run // Use A in B in C in D
//   * }}}
//   *
//   * @param ctx Shaded container function.
//   * @tparam T Result type
//   * @return
//   */
//  def shade[T](ctx: LocalizedContainer => T): T =
//    new HiddenContainerScope(ctx)(_c.shading)
//
//  def closed[T](ctx: LocalizedContainer => T): T =
//    new HiddenContainerScope(ctx)(ContainerImpl())
//
//  /**
//   * Gets an indexer for registering new dependencies.
//   * By default, the dependency priority is set to maximum.
//   *
//   * @param x        new dependency
//   * @param priority Injection priority.
//   * @tparam T new dependency type
//   * @return
//   */
//  protected def narrow[T: Type](
//                                        x: T,
//                                        priority: InjectionPriority = InjectionPriority.Primary
//                                      )(implicit ctn: Container): Indexer[T] = ctn.createIndexer(IndexedKey.from[T], x, priority)
  /**
   * Get accessible dependencies.
   *
   * The type information is resolved at compile time, but the injection object is finalized at runtime.
   *
   * @param access Accessor (This refers to itself)
   * @tparam T Injection type
   * @return
   */
  protected inline def inject[T]: refuel.container.provider.Lazy[T] = ${Macro.inject[T]}
}
