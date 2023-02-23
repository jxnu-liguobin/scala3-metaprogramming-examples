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

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.compiletime.*
import scala.deriving.Mirror
import scala.quoted.*

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/21
 */
object Bitlapx:

  inline def typeName[T]: String =
    inline scala.compiletime.erasedValue[T] match {
      case _: String  => classOf[String].getSimpleName
      case _: Int     => classOf[Int].getSimpleName
      case _: Char    => classOf[Char].getSimpleName
      case _: Boolean => classOf[Boolean].getSimpleName
      case _: Short   => classOf[Short].getSimpleName
      case _: Long    => classOf[Long].getSimpleName
      case _: Float   => classOf[Float].getSimpleName
      case _: Double  => classOf[Double].getSimpleName
      case _: Byte    => classOf[Byte].getSimpleName
      case _: Null    => "Null"
    }

  def fullName[T](using quotes: Quotes, tpe: Type[T]) = {
    import quotes.reflect.*
    TypeTree.of[T].symbol.fullName
  }

  def productElement[T](x: Any, idx: Int) =
    x.asInstanceOf[Product].productElement(idx).asInstanceOf[T]

  def to[T: Type, R: Type](f: Expr[T] => Expr[R])(using Quotes): Expr[T => R] =
    '{ (x: T) => ${ f('{ x }) } }

  def from[T: Type, R: Type](f: Expr[T => R])(using Quotes): Expr[T] => Expr[R] =
    (x: Expr[T]) => '{ $f($x) }

  inline def showTree[A](inline a: A): String = ${ showTreeImpl[A]('{ a }) }

  def showTreeImpl[A: Type](a: Expr[A])(using Quotes): Expr[String] =
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

  inline def labels[T](using m: Mirror.ProductOf[T]): List[String] =
    tupleTypeToString[m.MirroredElemLabels]

  transparent inline def summonInlineOpt[T]: Option[T] = summonFrom {
    case t: T => Some(t)
    case _    => None
  }

  inline def printAtTime(key: Expr[String], expr: Expr[Any])(using q: Quotes): Expr[Unit] =
    '{
      val start = ZonedDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd hh:mm:ss"))
      println("time [" + start + "] | " + ${ key } + " [" + ${ expr } + "]")
    }

  inline def timed[T](inline expr: T): T = ${ timedImpl('{ expr }) }

  private def timedImpl[T: Type](expr: Expr[T])(using Quotes): Expr[T] =
    '{
      val start = System.currentTimeMillis()
      try $expr
      finally {
        val end               = System.currentTimeMillis()
        val exprAsString      = ${ Expr(exprAsCompactString(expr)) }.replaceAll("\\s+", " ").trim()
        val exprAsStringShort = if (exprAsString.length > 120) exprAsString.take(120) + "..." else exprAsString
        println(s"Evaluating $exprAsStringShort took: ${end - start}ms")
      }
    }

  private def exprAsCompactString[T: Type](expr: Expr[T])(using ctx: Quotes): String = {
    import ctx.reflect._
    expr.asTerm match {
      case Inlined(_, _, Apply(method, params)) => s"${method.symbol.name}(${params.map(_.show).mkString(",")})"
      case _                                    => expr.show
    }
  }

  extension [T](t: T) {
    def show(using Showx[T]): String = summon[Showx[T]].show(t)
  }

  extension [T <: Product](self: T) {
    def debug: Unit = Debugger.debug(self)
  }
