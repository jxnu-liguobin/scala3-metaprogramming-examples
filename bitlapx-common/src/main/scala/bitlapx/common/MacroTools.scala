/*
 * Copyright (c) 2023 bitlap
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package bitlapx.common

import bitlapx.common.*

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.deriving.Mirror
import scala.annotation.tailrec
import scala.compiletime.*
import scala.quoted.*
import scala.reflect.{ classTag, ClassTag }

/** @author
 *    梦境迷离
 *  @version 1.0,2023/3/1
 */
object MacroTools {
  extension [T](t: T) def typeInfo = summon[TypeInfo[T]]

  extension [T, Out](using Encoder[T, Out])(t: T) def encode = summon[Encoder[T, Out]].encode(t)

  extension [T](using Decoder[T])(t: String) def decode = summon[Decoder[T]].decode(t)

  extension (tpe: Type[?])
    def fullName(using Quotes): String = {
      import quotes.reflect.*
      TypeRepr.of(using tpe).show(using Printer.TypeReprCode)
    }

  def runtimeName[T: ClassTag] =
    classTag[T].runtimeClass.getTypeName

  def productElement[T](x: Any, idx: Int) =
    x.asInstanceOf[Product].productElement(idx).asInstanceOf[T]

  def to[T: Type, R: Type](f: Expr[T] => Expr[R])(using Quotes): Expr[T => R] =
    '{ (x: T) => ${ f('{ x }) } }

  def from[T: Type, R: Type](f: Expr[T => R])(using Quotes): Expr[T] => Expr[R] =
    (x: Expr[T]) => '{ $f($x) }

  inline def showTree_[A](inline a: A): String = ${ showTree[A]('{ a }) }

  def showTree[A: Type](a: Expr[A])(using Quotes): Expr[String] =
    import quotes.reflect.*
    //    Expr(a.asTerm.show)
    Expr(Printer.TreeStructure.show(a.asTerm))

  inline def tupleTypeToString[A <: Tuple]: List[String] = inline erasedValue[A] match {
    case _: EmptyTuple => Nil
    case _: (head *: tail) =>
      val headStr = constValue[head].toString
      val tailStr = tupleTypeToString[tail]
      headStr :: tailStr
  }

  inline def labels[T](using m: Mirror.Of[T]): List[String] =
    tupleTypeToString[m.MirroredElemLabels]

  inline def labelsToList[T <: Tuple]: List[String] = inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => constValue[t].toString :: labelsToList[ts]
  }

  transparent inline def summonInlineOpt[T]: Option[T] = summonFrom {
    case t: T => Some(t)
    case _    => None
  }

  inline def debug(inline exprs: Any*): Unit = ${ debugImpl('{ exprs }) }

  private def debugImpl(exprs: Expr[Seq[Any]])(using q: Quotes): Expr[String] =
    import q.reflect._

    def showWithValue(e: Expr[_]): Expr[String] = '{ ${ Expr(e.show) } + " = " + $e }

    val stringExps: Seq[Expr[String]] = exprs match
      case Varargs(es) =>
        es.map { e =>
          e.asTerm match {
            case Literal(c: Constant) => Expr(c.value.toString)
            case _                    => showWithValue(e)
          }
        }
      case e => List(showWithValue(e))

    stringExps.reduceOption((e1, e2) => '{ ${ e1 } + ", " + ${ e2 } }).getOrElse('{ "" })

  private def exprAsCompactString[T: Type](expr: Expr[T])(using ctx: Quotes): String = {
    import ctx.reflect._
    expr.asTerm match {
      case Inlined(_, _, Apply(method, params)) => s"${method.symbol.name}(${params.map(_.show).mkString(",")})"
      case _                                    => expr.show
    }
  }

  inline def isExpectedReturnType[R: Type](using quotes: Quotes): quotes.reflect.Symbol => Boolean = { method =>
    import quotes.reflect.*
    val expectedReturnType = TypeRepr.of[R]
    method.tree match {
      case DefDef(_, _, typedTree, _) =>
        TypeRepr.of(using typedTree.tpe.asType) <:< expectedReturnType
      case _ => false
    }
  }

  inline def recurse[A <: Tuple, F[_]]: List[(String, F[Any])] =
    inline erasedValue[A] match {
      case _: (t *: ts) =>
        val name = TypeInfo.typeInfo[t].short
        name -> summonInline[F[t]].asInstanceOf[F[Any]] :: recurse[ts, F]
      case EmptyTuple => Nil
    }
}
