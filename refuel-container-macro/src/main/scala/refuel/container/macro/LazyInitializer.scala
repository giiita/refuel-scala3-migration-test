package refuel.container.`macro`

import refuel.container.`macro`.internal.DependencyRankings.ValDefModule_Expr
import refuel.container.`macro`.internal.{DependencyRankings, StaticDependencyExtractor}
import refuel.container.`macro`.internal.tools.LowLevelAPIConversionAlias
import refuel.container.{Container, DependencyPoolRef, IndexedKey, InjectionPool}
import refuel.container.provider.{Accessor, Lazy, TypedAcceptContext}
import refuel.container.provider.restriction.SymbolRestriction
import refuel.inject.InjectionPriority
import sun.reflect.generics.tree.TypeTree

import scala.annotation.tailrec
import scala.quoted._

object LazyInitializer extends LowLevelAPIConversionAlias {

  def init[T: Type](using q: Quotes): Expr[Lazy[T]] = {
    import q.reflect._
    val ip: Expr[InjectionPool] = Implicits.search(q.reflect.TypeRepr.of[InjectionPool]) match {
      case iss: ImplicitSearchSuccess => iss.tree.asExpr.asInstanceOf[Expr[InjectionPool]]
      case _: ImplicitSearchFailure => report.throwError("No found injection pool reference.")
    }
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
        val __refuel_result = ${injection[T](container, ip, access, false)}
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

  def injection[T: Type](cnt: Expr[Container], ip: Expr[InjectionPool], access: Expr[Accessor[_]], isAll: Boolean)(using q: Quotes): Expr[T] = {
    import q.reflect._
    val key = IndexedKey.from[T]

    val exists = '{
      ${cnt}.find[T, Accessor[_]](${key}, ${access})
    }

    val (candidates, isRuntimed) = StaticDependencyExtractor.searchInjectionCandidates[T]
    if (!isRuntimed) {
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
    } else {
      report.info(s"Ranking from [${candidates.map(_.symbol.name).mkString(",")}] and runtime classpath.")
      '{
        ${exists} getOrElse {
          ${runtimeConstruction[T](cnt, ip)} match {
            case (p, f) if f.size == 1 =>

              ${key}.synchronized {
                ${exists} getOrElse {
                  ${cnt}.createIndexer[T](${key}, f.head(p).value, p)
                    .indexing().value
                }
              }
          }
        }
      }
    }
  }


  private def runtimeConstruction[T: Type](cnt: Expr[Container], ip: Expr[InjectionPool])(using q: Quotes): Expr[(InjectionPriority, Seq[InjectionPriority => SymbolRestriction[T]])] = {
    val name = q.reflect.TypeTree.of[T].symbol.fullName
    q.reflect.TypeTree.of[T].tpe.asType match {
      case '[t] =>
        '{
          ${ip}.collect[T](Class.forName(${Expr(name)})).apply(${cnt}) getOrElse {
            throw new refuel.inject.exception.InjectDefinitionException(s"Cannot found " + ${Expr(name)} + "implementations.")
          }
        }
    }
  }
}
