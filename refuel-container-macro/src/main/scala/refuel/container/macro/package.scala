package refuel.container.`macro`

import refuel.container.IndexedKey
import refuel.container.provider.{Accessor, TypedAcceptContext}
import refuel.container.provider.restriction.SymbolRestriction

import scala.quoted._

given AccessorTypeAcceptContext: TypedAcceptContext[Accessor[_]] with
  override def accepted: SymbolRestriction[_] => Accessor[_] => Boolean = { x => y =>
    x.isOpen || x.acceptedClass(y.t.getClass) || x.acceptedInstance(y.t)
  }

given ClassTypeAcceptContext: TypedAcceptContext[Class[_]] with
  override def accepted: SymbolRestriction[_] => Class[_] => Boolean = { x => y => x.isOpen || x.acceptedClass(y) }

given SymbolToExpr: ToExpr[IndexedKey] with {
  def apply(x: IndexedKey)(using q: Quotes) = {
    import quotes.reflect._
    '{
      scala.Symbol(${Expr(x.toString())})
    }
  }
//      Select.overloaded(This(q.reflect.TypeTree.of[Symbol].symbol), "apply", Nil, List(Literal(StringConstant(x.toString())))).asExprOf[T]
}
