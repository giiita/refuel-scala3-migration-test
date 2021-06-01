package refuel.container

import scala.quoted._

object IndexedKey {
  given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)

  import scala.quoted.Type
  def fromClass[T](clz: Class[T]): IndexedKey = {
    scala.Symbol(clz.getName)
  }
  def from[T <: AnyKind: Type](using q: Quotes): Expr[IndexedKey] = {
    import q.reflect._
    '{
      scala.Symbol(${Expr(q.reflect.TypeTree.of[T].symbol.fullName)})
    }
  }
}

type IndexedKey = Symbol
