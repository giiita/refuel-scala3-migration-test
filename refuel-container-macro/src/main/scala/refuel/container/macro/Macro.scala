package refuel.container.`macro`

import refuel.container.provider.{Lazy, Accessor}
import refuel.container.`macro`.LazyInitializer

import scala.quoted._

object Macro {

  def inject[T: Type](using q: Quotes): Expr[Lazy[T]] = {
    q.reflect.TypeTree.of[T].tpe.asType match {
      case '[Lazy[t]] =>
        val resultExprAlias = '{
          Lazy[Lazy[t]](${LazyInitializer.init[t]})
        }
        resultExprAlias.asInstanceOf[Expr[Lazy[T]]]
      case _ =>
        LazyInitializer.init[T]
    }
  }
}
