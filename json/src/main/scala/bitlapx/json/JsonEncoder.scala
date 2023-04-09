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
import bitlapx.json.annotation.*
import magnolia1.*

import scala.collection.immutable.ListMap
import scala.compiletime.*
import scala.deriving.*
import scala.deriving.Mirror
import scala.quoted.*

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/24
 */
trait JsonEncoder[A]:

  def encode(a: A): Json

  def asString(a: A): String = encode(a).asJsonString

  def asPrettyString(a: A): String = JsonPrettyPrinter.prettyPrintJson(encode(a).asJsonString)

end JsonEncoder

object JsonEncoder extends EncoderLowPriority1 with Derivation[JsonEncoder]:
  self =>

  override def join[A](ctx: CaseClass[Typeclass, A]): Typeclass[A] =
    if (ctx.params.isEmpty) { (_: A) =>
      Json.Obj(ListMap.empty)
    } else
      (a: A) =>
        // exclude in json ast
        val params = ctx.params.filterNot { param =>
          param.annotations.collectFirst { case _: jsonExclude =>
            ()
          }.isDefined
        }

        Json.Obj(
          params
            .foldLeft[ListMap[String, Json]](ListMap.empty) { case (chunk, param) =>
              val name = param.annotations.collectFirst { case jsonField(name) =>
                name
              }.getOrElse(param.label)
              val value = param.typeclass.encode(param.deref(a))
              if (value == Json.Null) chunk ++ ListMap(name -> Json.Null) // filter Json.Null?
              else chunk ++ ListMap(name -> value)
            }
        )

  override def split[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] = (a: T) =>
    ctx.choose(a) { sub =>
      val value = sub.typeclass.encode(sub.cast(a))
      Json.Obj(
        ListMap(
          sub.typeInfo.short -> value
        )
      )
    }

  inline def gen[A](using mirror: Mirror.Of[A]) = self.derived[A]

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
