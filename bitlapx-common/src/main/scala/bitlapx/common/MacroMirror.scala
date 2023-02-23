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
import scala.deriving.Mirror
import bitlapx.common.MacroUtils.*

/** @author
 *    梦境迷离
 *  @version 1.0,2023/3/1
 */
class MirrorElem[Q <: Quotes & Singleton, A](using val quotes: Q, val asType: Type[A])(
  val label: String,
  val typeRepr: quotes.reflect.TypeRepr
):
  import quotes.*

  def summon[F[_]: Type]: Expr[F[A]] =
    val repr = quotes.reflect.TypeRepr.of[F[A]]
    Expr
      .summon[F[A]]
      .getOrElse(quotes.reflect.report.errorAndAbort(s"Cannot summon ${repr.show}"))

  def valueOfConstant: Option[A] =
    Type.valueOfConstant[A]
end MirrorElem

sealed trait MacroMirror[Q <: Quotes & Singleton, A]:
  val quotes: Q
  val tpe: Type[A]

  import quotes.reflect.*

  def label: String
  def monoType: quotes.reflect.TypeRepr
  def elemLabels: List[String]
  def elemTypes: List[quotes.reflect.TypeRepr]

  def elems: List[MirrorElem[quotes.type, ?]] =
    given Quotes = quotes
    elemLabels.zip(elemTypes).map { case (label, elemType) =>
      elemType.asType match
        case '[t] =>
          new MirrorElem[quotes.type, t](using quotes)(label = label, typeRepr = elemType)
    }

  def summonAll[F[_]: Type]: List[Expr[F[Any]]] =
    elems.map(_.summon[F]).asInstanceOf[List[Expr[F[Any]]]]

  protected def mirrorTypeLabel: String =
    this match
      case _: MacroMirror.SingletonMacroMirror[Q, A] => "Singleton"
      case _: MacroMirror.ProductMacroMirror[Q, A]   => "Product"
      case _: MacroMirror.SumMacroMirror[Q, A]       => "Sum"

  override def toString: String =
    s"""${mirrorTypeLabel}MacroMirror(
       |  label = $label,
       |  monoType = ${monoType.show}
       |  elemLabels = $elemLabels
       |  elemTypes = ${elemTypes.map(_.show)}
       |)""".stripMargin
end MacroMirror

object MacroMirror:

  abstract class SumMacroMirror[Q <: Quotes & Singleton, A](using
    override val quotes: Q,
    override val tpe: Type[A]
  ) extends MacroMirror[Q, A]:
    import quotes.reflect.*

    def ordinal[T: Type]: Int =
      ordinal(TypeRepr.of[T])

    def ordinal(repr: TypeRepr): Int =
      elemTypes.indexWhere(repr =:= _) match
        case -1 => report.errorAndAbort(s"Type $repr is not a member of $monoType")
        case n  => n
  end SumMacroMirror

  abstract class ProductMacroMirror[Q <: Quotes & Singleton, A](using
    override val quotes: Q,
    override val tpe: Type[A]
  ) extends MacroMirror[Q, A]:
    import quotes.reflect.*

    def construct(args: Seq[quotes.reflect.Term]): Expr[A] =
      Term
        .companionOf(monoType)
        .call("apply", monoType.typeArgs, args.toList)
        .asExprOf[A]
  end ProductMacroMirror

   abstract class SingletonMacroMirror[Q <: Quotes & Singleton, A](using
     override val quotes: Q,
     override val tpe: Type[A]
   )  extends ProductMacroMirror[Q, A]:
        import quotes.reflect.*
        override def construct(args: Seq[quotes.reflect.Term]): Expr[A] = expr
        def expr: Expr[A] = Ref(monoType.termSymbol).asExprOf[A]
   end SingletonMacroMirror


    def summon[A: Type](using quotes: Quotes): MacroMirror[quotes.type, A] =
      import quotes.reflect.*
      Expr
        .summon[Mirror.Of[A]]
        .getOrElse(report.errorAndAbort(s"Cannot summon Mirror.Of[${TypeRepr.of[A].show}]")) match

        case '{
              $m: Mirror.Singleton {
                type MirroredMonoType   = monoType
                type MirroredLabel      = label
                type MirroredElemTypes  = elemTypes
                type MirroredElemLabels = elemLabels
              }
            } =>
          new SingletonMacroMirror[quotes.type, A]:
            val label      = Type.valueOfConstant[label].get.asInstanceOf[String]
            val monoType   = TypeRepr.of[monoType]
            val elemLabels = TypeRepr.of[elemLabels].tupleToList.map(_.valueAs[String])
            val elemTypes  = TypeRepr.of[elemTypes].tupleToList

        case '{
              $m: Mirror.ProductOf[A] {
                type MirroredMonoType   = monoType
                type MirroredLabel      = label
                type MirroredElemTypes  = elemTypes
                type MirroredElemLabels = elemLabels
              }
            } =>
          new ProductMacroMirror[quotes.type, A]:
            val label      = Type.valueOfConstant[label].get.asInstanceOf[String]
            val monoType   = TypeRepr.of[monoType]
            val elemLabels = TypeRepr.of[elemLabels].tupleToList.map(_.valueAs[String])
            val elemTypes  = TypeRepr.of[elemTypes].tupleToList

        case '{
              $m: Mirror.SumOf[A] {
                type MirroredMonoType   = monoType
                type MirroredLabel      = label
                type MirroredElemTypes  = elemTypes
                type MirroredElemLabels = elemLabels
              }
            } =>
          new SumMacroMirror[quotes.type, A]:
            val label      = Type.valueOfConstant[label].get.asInstanceOf[String]
            val monoType   = TypeRepr.of[monoType]
            val elemLabels = TypeRepr.of[elemLabels].tupleToList.map(_.valueAs[String])
            val elemTypes  = TypeRepr.of[elemTypes].tupleToList
    end summon

    def summonProduct[A: Type](using quotes: Quotes): ProductMacroMirror[quotes.type, A] =
      import quotes.reflect.*
      summon[A] match
        case m: ProductMacroMirror[quotes.type, A] => m
        case _ => quotes.reflect.report.errorAndAbort(s"Cannot summon ProductMacroMirror[${TypeRepr.of[A].show}]")
    end summonProduct

end MacroMirror
