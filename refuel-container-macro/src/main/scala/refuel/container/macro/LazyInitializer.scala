package refuel.container.`macro`

import refuel.container.`macro`.internal.DependencyRankings.ValDefModule_Expr
import refuel.container.`macro`.internal.{DependencyRankings, StaticDependencyExtractor}
import refuel.container.`macro`.internal.tools.LowLevelAPIConversionAlias
import refuel.container.{Container, DependencyPoolRef, IndexedKey}
import refuel.container.provider.{Accessor, Lazy, TypedAcceptContext}
import refuel.container.provider.restriction.SymbolRestriction
import refuel.inject.InjectionPriority
import sun.reflect.generics.tree.TypeTree

import scala.annotation.tailrec
import scala.quoted._

object LazyInitializer extends LowLevelAPIConversionAlias {

  def init[T: Type](using q: Quotes): Expr[Lazy[T]] = {
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
        val __refuel_result = ${forceInitInject[T](container, access, false)}
        __refuel_result match {
          case value: DependencyPoolRef[Container] =>
            value.__refuel_cRef = Some(ctn)
        }
        __refuel_result
      }
    }

    '{
      Lazy[T](${injectionRf}.apply(${container}))
    }
  }

  def forceInitInject[T: Type](cnt: Expr[Container], access: Expr[Accessor[_]], isAll: Boolean)(using q: Quotes): Expr[T] = {
    import q.reflect._
    val key = IndexedKey.from[T]

    val exists = '{
    ${cnt}.find[T, Accessor[_]](${key}, ${access})
    }

    val candidates = StaticDependencyExtractor.searchInjectionCandidates[T]()
    {
      val (priority, ranked) = DependencyRankings(candidates)
      val rankedOne = DependencyRankings.generateExpr[T](ranked)
      '{
      ${exists} getOrElse {
        ${key}.synchronized {
          ${exists} getOrElse {
            ${cnt}.createIndexer[T](${key}, ${rankedOne}, ${priority})
              .indexing().value
          }
        }
      }
      }
    }
  }

  def maybeInject[T: Type](cnt: Expr[Container], access: Expr[Accessor[_]], isAll: Boolean)(using q: Quotes): Expr[Option[T]] = {
    import q.reflect._
    val key = IndexedKey.from[T]

    val exists: Expr[Option[T]] = '{
    ${cnt}.find[T, Accessor[_]](${key}, ${access})
    }

    val candidates = StaticDependencyExtractor.searchInjectionCandidates[T](true)

    if (candidates.isEmpty) {
      exists
    } else {
      val (priority, ranked) = DependencyRankings(candidates)
      val rankedOne = DependencyRankings.generateExpr[T](ranked)
      '{
      ${exists} orElse {
        ${key}.synchronized {
          ${exists} orElse {
            Some(
              ${cnt}.createIndexer[T](${key}, ${rankedOne}, ${priority})
                .indexing().value
            )
          }
        }
      }
      }
    }
  }

  def all[T: Type](cnt: Expr[Container], access: Expr[Accessor[_]])(using q: Quotes): Expr[Iterable[T]] = {
    import q.reflect._
    val key = IndexedKey.from[T]

    val exists = '{
    ${cnt}.fully[T, Accessor[_]](${key}, ${access})
    }

    val candidates = StaticDependencyExtractor.searchInjectionCandidates[T](true)

    if (candidates.isEmpty) {
      exists
    } else {
      val (priority, ranked) = DependencyRankings(candidates)
      val rankedOne = DependencyRankings.generateExpr[T](ranked)
      '{
        val __refuel_x = ${exists}
        if (__refuel_x.isEmpty) {
          ${key}.synchronized {
            val __refuel_y = ${exists}
            if (__refuel_y.isEmpty) {
              
            } else __refuel_y
          }
        } else x
      ${exists} getOrElse {
        ${key}.synchronized {
          ${exists} getOrElse {
            Some(
              ${cnt}.createIndexer[T](${key}, ${rankedOne}, ${priority})
                .indexing().value
            )
          }
        }
      }
      }
    }
  }
}
