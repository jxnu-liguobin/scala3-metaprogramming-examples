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

import magnolia1.*

import scala.deriving.Mirror
import scala.deriving.Mirror.Of

/** shows one type as another, often as a string
 *
 *  Note that this is a more general form of `Show` than is usual, as it permits the return type to be something other
 *  than a string.
 */
trait Encoder[T, Out]:
  def encode(value: T): Out
end Encoder

trait GenericShow[Out] extends AutoDerivation[[X] =>> Encoder[X, Out]]:

  def joinElems(typeName: String, strings: Seq[String]): Out
  def prefix(s: String, out: Out): Out

  def join[T](ctx: CaseClass[Typeclass, T]): Encoder[T, Out] = { value =>
    if ctx.isValueClass then
      val param = ctx.params.head
      param.typeclass.encode(param.deref(value))
    else
      val paramStrings = ctx.params.map { param =>
        val attribStr =
          if (param.annotations.isEmpty && param.inheritedAnnotations.isEmpty)
            ""
          else {
            (param.annotations ++ param.inheritedAnnotations).distinct
              .mkString("{", ",", "}")
          }

        val tpeAttribStr =
          if (param.typeAnnotations.isEmpty) ""
          else {
            param.typeAnnotations.mkString("{", ",", "}")
          }

        val shown = if (isSensitive(param.label)) "***" else param.typeclass.encode(param.deref(value))
        s"${param.label}$attribStr$tpeAttribStr=$shown"
      }

      val anns          = (ctx.annotations ++ ctx.inheritedAnnotations).distinct
      val annotationStr = if (anns.isEmpty) "" else anns.mkString("{", ",", "}")

      val tpeAnns = ctx.typeAnnotations
      val typeAnnotationStr =
        if (tpeAnns.isEmpty) "" else tpeAnns.mkString("{", ",", "}")

      joinElems(
        ctx.typeInfo.short + typeArgsString(
          ctx.typeInfo
        ) + annotationStr + typeAnnotationStr,
        paramStrings
      )
  }

  private def typeArgsString(typeInfo: TypeInfo): String =
    if typeInfo.typeParams.isEmpty then ""
    else
      typeInfo.typeParams
        .map(arg => s"${arg.short}${typeArgsString(arg)}")
        .mkString("[", ",", "]")

  override def split[T](ctx: SealedTrait[Typeclass, T]): Encoder[T, Out] =
    (value: T) =>
      ctx.choose(value) { sub =>
        val anns = (sub.annotations ++ sub.inheritedAnnotations).distinct

        val annotationStr =
          if (anns.isEmpty) "" else anns.mkString("{", ",", "}")

        prefix(annotationStr, sub.typeclass.encode(sub.value))
      }

  private def isSensitive(n: String): Boolean = {
    val nn = n.toLowerCase
    nn.contains("token") || nn.contains("apikey") || nn.contains("password")
  }

end GenericShow

object Encoder extends GenericShow[String]:

  def prefix(s: String, out: String): String = s + out

  def joinElems(typeName: String, params: Seq[String]): String =
    params.mkString(s"$typeName(", ",", ")")

  given Encoder[String, String]     = identity(_)
  given Encoder[Int, String]        = _.toString
  given Encoder[Long, String]       = _.toString + "L"
  given Encoder[Boolean, String]    = _.toString
  given Encoder[Short, String]      = _.toString
  given Encoder[Float, String]      = _.toString
  given Encoder[Double, String]     = _.toString
  given Encoder[BigDecimal, String] = _.toString()
  given Encoder[Byte, String]       = _.toString
  given Encoder[Char, String]       = _.toString
  given [A](using A: Encoder[A, String]): Encoder[Seq[A], String] =
    _.iterator.map(A.encode).mkString("[", ",", "]")
