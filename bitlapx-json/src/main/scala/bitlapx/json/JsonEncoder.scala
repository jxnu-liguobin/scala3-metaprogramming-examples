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

package bitlapx.json

import ast.*
import bitlapx.common.MacroTools.*
import bitlapx.common.TypeInfo
import bitlapx.common.TypeInfo.typeInfo
import bitlapx.json.annotation.*

import scala.collection.immutable.ListMap
import scala.compiletime.*
import scala.deriving.*
import scala.deriving.Mirror
import scala.quoted.*
import scala.reflect.{ classTag, ClassTag }

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/24
 */
trait JsonEncoder[A]:

  def encode(a: A): Json

end JsonEncoder

object JsonEncoder extends EncoderLowPriority1:

  def apply[A](using a: JsonEncoder[A]): JsonEncoder[A] = a

  given javaBigDecimalEncoder: JsonEncoder[java.math.BigDecimal] = (v: java.math.BigDecimal) => Json.Num(v)

  given JsonEncoder[Symbol] = (v: Symbol) => Json.Str(v.name)

  given JsonEncoder[Boolean] = (v: Boolean) => Json.Bool(v)

  given JsonEncoder[String] = (v: String) => Json.Str(v)

  given JsonEncoder[Short] = (v: Short) => Json.Num(v)

  given JsonEncoder[Long] = (v: Long) => Json.Num(v)

  given JsonEncoder[Byte] = (v: Byte) => Json.Num(v)

  given JsonEncoder[Int] = (v: Int) => Json.Num(v)

  given JsonEncoder[BigDecimal] = (v: BigDecimal) => Json.Num(v)

  given JsonEncoder[Float] = (v: Float) => Json.Num(v)

  given JsonEncoder[Double] = (v: Double) => Json.Num(v)

  given [A](using js: JsonEncoder[A]): JsonEncoder[List[A]] = list[A]

  given option[A](using jsonEncoder: JsonEncoder[A]): JsonEncoder[Option[A]] = (a: Option[A]) =>
    a match
      case None    => Json.Null
      case Some(a) => jsonEncoder.encode(a)

  given either[A, B](using jsonEncoderA: JsonEncoder[A], jsonEncoderB: JsonEncoder[B]): JsonEncoder[Either[A, B]] =
    (a: Either[A, B]) =>
      a match
        case Left(a) =>
          Json.Obj(
            ListMap(
              "Left" -> jsonEncoderA.encode(a)
            )
          )
        case Right(b) =>
          Json.Obj(
            ListMap(
              "Right" -> jsonEncoderB.encode(b)
            )
          )

  inline given derived[V](using m: Mirror.Of[V]): JsonEncoder[V] = (v: V) => {
    val pans: Map[String, List[Any]] = TypeInfo.paramAnns[V].to(Map)
    inline m match {
      case sum: Mirror.SumOf[V] =>
        throw new Exception(s"Not support sum type")
      case _: Mirror.ProductOf[V] =>
        Json.Obj(
          toListMap[m.MirroredElemTypes, m.MirroredElemLabels, V](v, 0)(pans)
        )
    }
  }

  private inline def toListMap[T, L, V](v: V, i: Int)(pans: Map[String, List[Any]]): ListMap[String, Json] =
    inline erasedValue[(T, L)] match
      case _: (EmptyTuple, EmptyTuple) => ListMap.empty
      case _: (t *: ts, l *: ls) =>
        val js      = summonInline[JsonEncoder[t]]
        val label   = constValue[l].asInstanceOf[String]
        val value   = js.encode(productElement[t](v, i))
        val exclude = pans.get(label).fold(false)(as => as.collectFirst { case _: jsonExclude => () }.isDefined)
        val name: String =
          pans.get(label).fold(label)(as => as.collectFirst { case jsonField(name) => name }.getOrElse(label))
        if exclude then toListMap[ts, ls, V](v, i + 1)(pans)
        else ListMap(name -> value) ++ toListMap[ts, ls, V](v, i + 1)(pans)
