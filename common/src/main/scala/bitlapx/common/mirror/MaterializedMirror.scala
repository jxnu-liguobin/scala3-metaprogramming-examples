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

package bitlapx.common.mirror

import bitlapx.common.*

import scala.annotation.tailrec
import scala.deriving.Mirror
import scala.quoted.*

/** @author
 *    梦境迷离
 *  @version 1.0,2023/3/3
 */
final class MaterializedMirror[Q <: Quotes & Singleton] private (using val quotes: Q)(
  val mirroredType: quotes.reflect.TypeRepr,
  val mirroredMonoType: quotes.reflect.TypeRepr,
  val mirroredElemTypes: List[quotes.reflect.TypeRepr],
  val mirroredLabel: String,
  val mirroredElemLabels: List[String]
)

// https://github.com/typelevel/shapeless-3/blob/main/modules/deriving/src/main/scala/shapeless3/deriving/internals/reflectionutils.scala
object MaterializedMirror:

  def createOrAbort[A: Type](mirror: Expr[Mirror.Of[A]])(using Quotes): MaterializedMirror[quotes.type] =
    create(mirror).fold(memberName => Failure.abort(Failure.MirrorMaterialization(summon, memberName)), identity)

  private def create(mirror: Expr[Mirror])(using Quotes): Either[String, MaterializedMirror[quotes.type]] =
    import quotes.reflect.*
    val mirrorTpe = mirror.asTerm.tpe.widen
    for {
      mirroredType       <- findMemberType(mirrorTpe, "MirroredType")
      mirroredMonoType   <- findMemberType(mirrorTpe, "MirroredMonoType")
      mirroredElemTypes  <- findMemberType(mirrorTpe, "MirroredElemTypes")
      mirroredLabel      <- findMemberType(mirrorTpe, "MirroredLabel")
      mirroredElemLabels <- findMemberType(mirrorTpe, "MirroredElemLabels")
    } yield {
      val elemTypes                           = tupleTypeElements(mirroredElemTypes)
      val ConstantType(StringConstant(label)) = mirroredLabel: @unchecked
      val elemLabels = tupleTypeElements(mirroredElemLabels).map { case ConstantType(StringConstant(l)) => l }
      MaterializedMirror(mirroredType, mirroredMonoType, elemTypes, label, elemLabels)
    }

  private def tupleTypeElements(using Quotes)(tp: quotes.reflect.TypeRepr): List[quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    @tailrec def loop(tp: TypeRepr, acc: List[TypeRepr]): List[TypeRepr] = tp match {
      case AppliedType(pairTpe, List(hd: TypeRepr, tl: TypeRepr)) => loop(tl, hd :: acc)
      case _                                                      => acc
    }
    loop(tp, Nil).reverse
  end tupleTypeElements

  private def lowBound(using Quotes)(tp: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    tp match {
      case tp: TypeBounds => tp.low
      case tp             => tp
    }
  end lowBound

  private def findMemberType(using
    Quotes
  )(tp: quotes.reflect.TypeRepr, name: String): Either[String, quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    tp match {
      case Refinement(_, `name`, tp) => Right(lowBound(tp))
      case Refinement(parent, _, _)  => findMemberType(parent, name)
      case AndType(left, right)      => findMemberType(left, name).orElse(findMemberType(right, name))
      case _                         => Left(name)
    }
