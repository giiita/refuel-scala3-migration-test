package refuel.container.`macro`.internal

import refuel.container.Container
import refuel.container.`macro`.internal.DependencyRankings.isDependencyPoolRef
import refuel.container.`macro`.internal.tools.LowLevelAPIConversionAlias
import refuel.inject.InjectionPriority.Default
import refuel.inject.{AutoInjectBase, Inject, InjectableTag, InjectionPriority}

import scala.annotation.tailrec
import scala.quoted._

object DependencyRankings extends LowLevelAPIConversionAlias {
  /* Injection priority config type tag */
  private[this] def InjectionPriorityConfigType(using q: Quotes) = q.reflect.TypeRepr.of[Inject[?]]
  private[this] def DefaultType(using q: Quotes)                 = q.reflect.TypeRepr.of[Default]

  def apply(using q: Quotes)(cands: Iterable[q.reflect.TypeTree]): (Expr[InjectionPriority], Iterable[q.reflect.TypeTree]) = {
    import q.reflect._
    val (priorityGroup, pAndSyms) = cands.map { sym =>
      sym.symbol.annotations.find(_.tpe <:< InjectionPriorityConfigType)
        .flatMap(_.tpe match {
          case AppliedType(_, List(x)) => Some(x)
          case _ => None
        }).getOrElse(DefaultType) -> sym
    }.groupBy(_._1).reduce { (a, b) =>
      val maybe = {
        a._1.baseType(b._1.typeSymbol) match {
          case x if x.typeSymbol.isNoSymbol => b._1.baseType(a._1.typeSymbol)
          case x => x
        }
      }
      if (maybe =:= a._1) a else b
    }

    {
      priorityGroup.typeSymbol.companionModule.tree match {
        case ValDef(_, tree, _) =>
          This(tree.symbol).asExpr.asExprOf[InjectionPriority]
      }
    } -> pAndSyms.map(_._2)
  }

  def generateExpr[T: Type](using q: Quotes)(samePriorities: Iterable[q.reflect.TypeTree]): Expr[T] = {
    import q.reflect._
    if (samePriorities.size > 1) {
      report.throwError(s"Invalid dependency definition. There must be one automatic injection per priority. But found [${samePriorities.map(_.symbol.fullName).mkString(", ")}]")
    } else {
      report.info(s"${samePriorities.head.symbol.fullName} will be used.")
      build[T](samePriorities.head.symbol)
    }
  }

  @tailrec
  private[this] def isDependencyPoolRef(using q: Quotes)(sym: q.reflect.Symbol): q.reflect.Symbol = {
    sym.tree match {
      case dotty.tools.dotc.ast.Trees.TypeDef(_, _) if q.reflect.TypeIdent(sym).tpe.<:<(q.reflect.TypeTree.of[refuel.container.DependencyPoolRef[Container]].tpe) =>
        sym
      case _ => isDependencyPoolRef(sym.owner)
    }
  }

  private[this] def build[T: Type](using q: Quotes)(target: q.reflect.Symbol): Expr[T] = {
    import q.reflect._
    if (target.isValDef && target.flags.is(Flags.Module)) {
      ValDefModule_Expr(target).asExprOf[T]
    } else {
      Select.overloaded(
        New(q.reflect.TypeTree.of[T]),
        "<init>",
        Nil,
        target.primaryConstructor.paramSymss.flatMap(_.map { x =>
          x.tree match {
            case ValDef(_, tree, _) =>
              tree.tpe.asType match {
                case '[t] =>
                  Select.unique(
                    Select.unique(This(isDependencyPoolRef(q.reflect.Symbol.spliceOwner)), "inject").appliedToType(tree.tpe).asExpr.asTerm,
                    "_provide"
                  ).asExpr.asTerm
              }
          }
        })
      ).asExprOf[T]
    }
  }
}
