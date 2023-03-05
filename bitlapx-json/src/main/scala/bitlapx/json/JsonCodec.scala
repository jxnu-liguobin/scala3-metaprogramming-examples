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
import scala.collection.immutable.*
import scala.collection.immutable.ListMap
import scala.compiletime.*
import scala.deriving.*
import scala.reflect.ClassTag
import scala.runtime.Tuples
import scala.util.*

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/23
 */
final class JsonCodec[V](e: JsonEncoder[V], d: JsonDecoder[V]):
  def toJson(v: V): Json              = e.encode(v)
  def fromJson(json: Json): Result[V] = d.decode(json)
end JsonCodec

object JsonCodec extends CodecLowPriority0:

  def apply[A](encoder: JsonEncoder[A], decoder: JsonDecoder[A]): JsonCodec[A] = new JsonCodec(encoder, decoder)

  inline def apply[V](using js: JsonCodec[V]): JsonCodec[V] = js

  inline given derived[V](using m: Mirror.Of[V], e: JsonEncoder[V], d: JsonDecoder[V]): JsonCodec[V] =
    new JsonCodec[V](e, d)

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
