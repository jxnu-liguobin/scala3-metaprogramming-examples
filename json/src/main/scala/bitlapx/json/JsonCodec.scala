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

import bitlapx.json.ast.*

import scala.collection.immutable
import scala.collection.immutable.ListMap
import scala.compiletime.*
import scala.deriving.*

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/23
 */
final class JsonCodec[A](e: JsonEncoder[A], d: JsonDecoder[A]):
  def toJson(v: A): Json              = e.encode(v)
  def fromJson(json: Json): Result[A] = d.decode(json)
  def asString(a: A): String          = toJson(a).asJsonString
  def asPrettyString(a: A): String    = JsonPrettyPrinter.prettyPrintJson(toJson(a).asJsonString)
end JsonCodec

object JsonCodec extends CodecLowPriority0:

  def apply[A](encoder: JsonEncoder[A], decoder: JsonDecoder[A]): JsonCodec[A] = new JsonCodec(encoder, decoder)

  inline def apply[A](using js: JsonCodec[A]): JsonCodec[A] = js

  inline given derived[A](using e: JsonEncoder[A], d: JsonDecoder[A]): JsonCodec[A] =
    new JsonCodec[A](e, d)

  given JsonCodec[String] = new JsonCodec[String](JsonEncoder[String], JsonDecoder[String])
  given JsonCodec[Int]    = new JsonCodec[Int](JsonEncoder[Int], JsonDecoder[Int])
  given JsonCodec[Short]  = new JsonCodec[Short](JsonEncoder[Short], JsonDecoder[Short])
  given JsonCodec[Long]   = new JsonCodec[Long](JsonEncoder[Long], JsonDecoder[Long])
  given JsonCodec[Float]  = new JsonCodec[Float](JsonEncoder[Float], JsonDecoder[Float])
  given JsonCodec[Double] = new JsonCodec[Double](JsonEncoder[Double], JsonDecoder[Double])
  given JsonCodec[Byte]   = new JsonCodec[Byte](JsonEncoder[Byte], JsonDecoder[Byte])
  given JsonCodec[BigDecimal] =
    new JsonCodec[BigDecimal](JsonEncoder[BigDecimal], JsonDecoder[BigDecimal])

  given javaBigDecimal: JsonCodec[java.math.BigDecimal] =
    new JsonCodec[java.math.BigDecimal](JsonEncoder.javaBigDecimalEncoder, JsonDecoder.javaBigDecimalDecoder)

  given JsonCodec[Boolean] = new JsonCodec[Boolean](JsonEncoder[Boolean], JsonDecoder[Boolean])
  given JsonCodec[Symbol]  = new JsonCodec[Symbol](JsonEncoder[Symbol], JsonDecoder[Symbol])

  given [A: JsonEncoder: JsonDecoder]: JsonCodec[Option[A]] =
    JsonCodec(JsonEncoder.option(using JsonEncoder[A]), JsonDecoder.option(using JsonDecoder[A]))

  given [A: JsonEncoder: JsonDecoder, B: JsonEncoder: JsonDecoder]: JsonCodec[Either[A, B]] =
    JsonCodec(
      JsonEncoder.either(using JsonEncoder[A], JsonEncoder[B]),
      JsonDecoder.either(using JsonDecoder[A], JsonDecoder[B])
    )
