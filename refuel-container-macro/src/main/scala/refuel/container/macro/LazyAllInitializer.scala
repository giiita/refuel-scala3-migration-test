package refuel.container.`macro`

import refuel.container.`macro`.internal.DependencyRankings.ValDefModule_Expr
import refuel.container.`macro`.internal.tools.LowLevelAPIConversionAlias
import refuel.container.`macro`.internal.{DependencyRankings, StaticDependencyExtractor}
import refuel.container.provider.restriction.SymbolRestriction
import refuel.container.provider.{Accessor, Lazy, TypedAcceptContext}
import refuel.container.{Container, DependencyPoolRef, IndexedKey}
import refuel.inject.InjectionPriority
import sun.reflect.generics.tree.TypeTree

import scala.annotation.tailrec
import scala.quoted._

object LazyAllInitializer extends LowLevelAPIConversionAlias {

  def init[T: Type](using q: Quotes): Expr[Lazy[Iterable[T]]] = {
    import q.reflect._
    val access: Expr[Accessor[_]] = Implicits.search(q.reflect.TypeRepr.of[Accessor[_]]) match {
      case iss: ImplicitSearchSuccess => iss.tree.asExpr.asInstanceOf[Expr[Accessor[_]]]
      case _: ImplicitSearchFailure => report.throwError("No found accessor.")
    }
    val container: Expr[Container] = Implicits.search(q.reflect.TypeRepr.of[Container]) match {
      case iss: ImplicitSearchSuccess => iss.tree.asExpr.asInstanceOf[Expr[Container]]
      case _: ImplicitSearchFailure => report.throwError("No found container reference.")
    }
    val injectionRf = '{
      (ctn: Container) => {
        ${all[T](container, access)}.map { value =>
          value match {
            case x: DependencyPoolRef[Container] =>
              x.__refuel_cRef = Some(ctn)
          }
          value
        }
      }
    }

    '{
      Lazy[Iterable[T]](${injectionRf}.apply(${container}))
    }
  }

  def all[T: Type](cnt: Expr[Container], access: Expr[Accessor[_]])(using q: Quotes): Expr[Iterable[T]] = {
    import q.reflect._
    val key = IndexedKey.from[T]

    val exists: Expr[Iterable[T]] = '{
      ${cnt}.fully[T, Accessor[_]](${key}, ${access})
    }

    val emptiableCands = StaticDependencyExtractor.searchInjectionCandidates[T]

    if (emptiableCands.isEmpty) {
      exists
    } else {
      DependencyRankings(emptiableCands).fold(
        report.throwError(
          s"Can't find a dependency registration of ${q.reflect.TypeTree.of[T].symbol.fullName}. Injection from runtime classpath must be given @RecognizedDynamicInjection."
        )
      ) {
        case (priority, ranked) =>
          val rankedAll = DependencyRankings.generateExpr[T](ranked)
          '{
            val __refuel_x = ${exists}
            if (__refuel_x.isEmpty) {
              ${key}.synchronized {
                val __refuel_y = ${exists}
                if (__refuel_y.isEmpty) {
                  ${rankedAll}
                } else __refuel_y
              }
            } else __refuel_x
          }
      }
    }
  }
}
