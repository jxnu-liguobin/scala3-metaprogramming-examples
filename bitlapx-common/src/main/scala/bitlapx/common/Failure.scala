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

import bitlapx.common.field.Fields

import scala.quoted.*

/** @author
 *    梦境迷离
 *  @version 1.0,2023/3/3
 */
sealed trait Failure:
  def position(using Quotes): quotes.reflect.Position =
    quotes.reflect.Position.ofMacroExpansion
  def render(using Quotes): String
end Failure

object Failure:

  def abort(failure: Failure)(using Quotes): Nothing =
    quotes.reflect.report.errorAndAbort(failure.render, failure.position)

  opaque type Suggestion = String

  object Suggestion:
    def apply(text: String): Suggestion                    = text
    def all(head: String, tail: String*): List[Suggestion] = head :: tail.toList
    def fromFields(fields: Fields): List[Suggestion]       = fields.value.map(f => Suggestion(s"_.${f.name}"))
    def renderAll(suggestions: List[Suggestion]): String =
      suggestions.mkString("\n| • ", "\n| • ", "")
  end Suggestion

  final case class MirrorMaterialization(mirroredType: Type[?], notFoundTypeMemberName: String) extends Failure:
    override def render(using Quotes): String = {
      import quotes.reflect.*

      s"""
         |Mirror materialization for ${mirroredType.show} failed. 
         |Not found member type : '$notFoundTypeMemberName'.
        """.stripMargin
    }
  end MirrorMaterialization

  final case class InvalidFieldSelector(
    selector: Expr[Any],
    sourceTpe: Type[?],
    suggestedFields: List[Suggestion]
  ) extends Failure:
    override def position(using Quotes): quotes.reflect.Position = selector.pos

    override def render(using Quotes): String =
      s"""
         |'${selector.show}' is not a valid field selector for ${sourceTpe.show}.
         |Try one of these: ${Suggestion.renderAll(suggestedFields)}
        """.stripMargin
  end InvalidFieldSelector

  enum InvalidArgSelector extends Failure:
    override final def position(using Quotes): quotes.reflect.Position =
      this match
        case NotFound(selector, _, _)      => selector.pos
        case NotAnArgSelector(selector, _) => selector.pos

    override final def render(using Quotes): String =
      this match {
        case NotFound(_, argName, suggestedArgs) =>
          s"""
             |'_.$argName' is not a valid argument selector.
             |Try one of these: ${Suggestion.renderAll(suggestedArgs)}
        """.stripMargin
        case NotAnArgSelector(_, suggestedArgs) =>
          s"""
             |Not a valid argument selector.
             |Try one of these: ${Suggestion.renderAll(suggestedArgs)}
        """.stripMargin
      }
    end render

    case NotFound(selector: Expr[Any], argumentName: String, suggestedArgs: List[Suggestion])
    case NotAnArgSelector(selector: Expr[Any], suggestedArgs: List[Suggestion])
  end InvalidArgSelector

  extension (tpe: Type[?]) private def show(using Quotes): String = quotes.reflect.TypeRepr.of(using tpe).show

  extension (expr: Expr[Any])
    private def show(using Quotes): String                 = quotes.reflect.asTerm(expr).show
    private def pos(using Quotes): quotes.reflect.Position = quotes.reflect.asTerm(expr).pos
