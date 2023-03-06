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

import bitlapx.common.annotation.CollectAnnotations
import magnolia1.Macro.typeInfo
import magnolia1.{TypeInfo as MTypeInfo, *}

import scala.annotation.tailrec
import scala.compiletime.erasedValue
import scala.deriving.Mirror
import scala.quoted.*
import scala.reflect.{ClassTag, classTag}

/** @author
 *    梦境迷离
 *  @version 1.0,2023/3/3
 */
trait TypeInfo[T]:
  def name: MTypeInfo
  def subtypeNames: Seq[MTypeInfo]
  def isValueClass: Boolean
  def isEnum: Boolean
  def paramNames: Map[Int, String]
  def annotations: List[Any]
end TypeInfo

object TypeInfo extends Derivation[TypeInfo]:

  inline def isObject[T]: Boolean = ${ isObject[T] }

  inline def isEnum[T]: Boolean = ${ isEnum[T] }

  inline def anns[T]: List[Any] = ${ anns[T] }

  inline def typeAnns[T]: List[Any] = ${ typeAnns[T] }

  inline def paramAnns[T]: List[(String, List[Any])] = ${ paramAnns[T] }

  inline def isValueClass[T]: Boolean = ${ isValueClass[T] }

  inline def defaultValue[T]: List[(String, Option[Any])] = ${ defaultValue[T] }

  def isObject[T: Type](using Quotes): Expr[Boolean] =
    import quotes.reflect.*

    Expr(TypeRepr.of[T].typeSymbol.flags.is(Flags.Module))

  def isEnum[T: Type](using Quotes): Expr[Boolean] =
    import quotes.reflect.*

    Expr(TypeRepr.of[T].typeSymbol.flags.is(Flags.Enum))

  def anns[T: Type](using Quotes): Expr[List[Any]] =
    new CollectAnnotations[T].anns

  def typeAnns[T: Type](using Quotes): Expr[List[Any]] =
    new CollectAnnotations[T].typeAnns

  def paramAnns[T: Type](using Quotes): Expr[List[(String, List[Any])]] =
    new CollectAnnotations[T].paramAnns

  def isValueClass[T: Type](using Quotes): Expr[Boolean] =
    import quotes.reflect.*
    Expr(
      TypeRepr.of[T].baseClasses.contains(Symbol.classSymbol("scala.AnyVal"))
    )

  def defaultValue[T: Type](using Quotes): Expr[List[(String, Option[Any])]] =
    import quotes.reflect._
    Expr.ofList(TypeRepr.of[T].typeSymbol.caseFields.map { case s =>
      Expr(s.name -> None)
    })

  inline def typeInfo[T]: MTypeInfo = Macro.typeInfo[T]

  inline def subTypes[T, SubtypeTuple <: Tuple](m: Mirror.SumOf[T], idx: Int = 0): List[MTypeInfo] =
    inline erasedValue[SubtypeTuple] match
      case _: EmptyTuple =>
        Nil
      case _: (s *: tail) =>
        typeInfo[s] :: subTypes[T, tail](m, idx + 1)
  end subTypes

  
  def choose[T](subtypes:List[MTypeInfo],v:T): Int =
    @tailrec def rec(ix: Int): Int =
    if ix < subtypes.length then
      val sub = subtypes(ix)
      val isLocalClass = v.getClass.isLocalClass && !v.getClass.isInterface
      val name = v.getClass.getSimpleName
      val className = if(isLocalClass) name.subSequence(0, name.indexOf("$")) else name 
      if sub.short == className then ix
      else rec(ix + 1)
    else
      throw new IllegalArgumentException(
        s"Invalid sub type of `${v.getClass.getSimpleName.stripSuffix("$")}`"
      )
    rec(0)

  override def join[T](ctx: CaseClass[TypeInfo, T]): TypeInfo[T] =
    new TypeInfo[T]:
      def name: MTypeInfo              = ctx.typeInfo
      def subtypeNames: Seq[MTypeInfo] = Nil
      def isValueClass: Boolean        = ctx.isValueClass
      def isEnum: Boolean              = false
      def paramNames: Map[Int, String] = ctx.params.map(p => p.index -> p.label).toMap
      def annotations: List[Any]       = ctx.annotations.toList

  override def split[T](ctx: SealedTrait[TypeInfo, T]): TypeInfo[T] =
    new TypeInfo[T]:
      def name: MTypeInfo              = ctx.typeInfo
      def subtypeNames: Seq[MTypeInfo] = ctx.subtypes.map(_.typeInfo)
      def isValueClass: Boolean        = false
      def isEnum: Boolean              = ctx.isEnum
      def paramNames: Map[Int, String] = Map.empty
      def annotations: List[Any]       = ctx.annotations.toList

  given fallback[T]: TypeInfo[T] =
    new TypeInfo[T]:
      def name: MTypeInfo              = MTypeInfo("", "Unknown Type", Seq.empty)
      def subtypeNames: Seq[MTypeInfo] = Nil
      def isValueClass: Boolean        = false
      def isEnum: Boolean              = false
      def paramNames: Map[Int, String] = Map.empty
      def annotations: List[Any]       = Nil
