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

import scala.quoted.*

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/21
 */
object Selector:

  final case class FieldPath[T, V](path: String)

  transparent inline def apply[T](inline f: T => Any): String = ${ nameImpl('{ f }) }

  private def nameImpl[T](f: Expr[T => Any])(using Type[T], Quotes): Expr[String] = {
    import quotes.reflect._
    val acc = new TreeAccumulator[String] {
      def foldTree(names: String, tree: Tree)(owner: Symbol): String = tree match {
        case Select(_, name) => name
        case _               => foldOverTree(names, tree)(owner)
      }
    }
    val fieldName                = acc.foldTree(null, f.asTerm)(Symbol.spliceOwner)
    val primaryConstructorFields = TypeTree.of[T].symbol.caseFields.map(_.name)
    if (!primaryConstructorFields.contains(fieldName))
      report.error(s"The field '$fieldName' is not one of the primary constructor parameter.", f)
    Expr(fieldName)
  }

  def select[T] = new SelectPartiallyApplied[T]

  final class SelectPartiallyApplied[T] {
    inline def apply[V](inline selector: T => V): FieldPath[T, V] =
      ${ selectImpl('{ selector }) }
  }

  private def selectImpl[T: Type, V: Type](selector: Expr[T => V])(using Quotes): Expr[FieldPath[T, V]] =
    import quotes.reflect.*

    def extractFields(term: Term, acc: List[String] = Nil): List[String] = term match {
      case Select(term, field) =>
        extractFields(term, field +: acc)
      case Ident(_) => acc
      case _ =>
        report.throwError(
          s"Expected selector, got $term"
        )
    }

    selector.asTerm match {
      case Inlined(_, _, Lambda(_, method)) =>
        '{
          FieldPath(${
            Expr(
              extractFields(method).mkString(".")
            )
          })
        }
      case _ =>
        report.throwError(
          s"Expected inlined lambda, got ${selector.show}"
        )
    }
