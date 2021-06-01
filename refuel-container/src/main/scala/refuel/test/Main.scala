package refuel.test

import refuel.container.provider.Lazy
import refuel.container.`macro`.LazyInitializer
import refuel.container.`macro`.Macro.inject
import refuel.inject.{AutoInject, InjectionPriority, Injector}
import refuel.container.provider.Accessor

import scala.language.implicitConversions

object Main extends Testing with AutoInject {
  val local: String = "local value"
   def main(args: Array[String]) = {
     println(aa.getClass.getName)
    println(bb)
  }

//  object Runnter {
//    inline def inject[T](inline str: String, an: Any): Lazy[String] =
//      ${LazyInitializer.initi[T]('str, 'an)}
//  }
}

trait Testing extends Injector {
  def aa: Foo = inject[Foo]
  def bb: Option[Foo] = {
    import scala.quoted._
//    import scala.quoted.staging._
//    given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)
//    staging.withQuotes {
    import refuel.container.`macro`.given
      __refuel_cRef.get.find(classOf[Foo])
//    }
  }
}

//@main def hello: Unit = {
// println(Injector.inject[Foo](null, null))
//}
//
//object Injector {
// inline def inject[T](inline str: Any, an: Any): Lazy[String] =
//  ${ LazyInitializer.init[T](null, null) }
//}

class Foo(bar: Lazy[Bar]) extends AutoInject

case class Bar() extends AutoInject
trait Mix


/**
 *
 * refuel.container.provider.Lazy.apply[refuel.test.Foo](
 *   (
 *     (ctn: refuel.container.Container) => {
         val __refuel_result: refuel.test.Foo = Testing.this.__refuel_c.find[refuel.test.Foo, refuel.container.provider.Accessor[_ >: scala.Nothing <: scala.Any]](scala.Symbol.apply("Symbol(Type.of[...])"), Testing.this.__refuel_accessor)(refuel.container.macro.AccessorTypeAcceptContext)
           .getOrElse[refuel.test.Foo](
             scala.Symbol.apply("Symbol(Type.of[...])").synchronized[refuel.test.Foo](
               Testing.this.__refuel_c.find[refuel.test.Foo, refuel.container.provider.Accessor[_ >: scala.Nothing <: scala.Any]](scala.Symbol.apply("Symbol(Type.of[...])"), Testing.this.__refuel_accessor)(refuel.container.macro.AccessorTypeAcceptContext)
                 .getOrElse[refuel.test.Foo](
                   {
                     val $3$: refuel.container.Container = Testing.this.__refuel_c
                     $3$.createIndexer[refuel.test.Foo](scala.Symbol.apply("Symbol(Type.of[...])"), new refuel.test.Foo(), _7.this, $3$.createIndexer$default$4[refuel.test.Foo])
                   }.indexing().value
                 )
             )
           )
         __refuel_result match {
           case value: refuel.container.DependencyPoolRef[refuel.container.Container] =>
             value.__refuel_c = ctn
           case _ =>
             ()
         }

         (__refuel_result: refuel.test.Foo)
       }
     ).apply(Testing.this.__refuel_c))
 */
