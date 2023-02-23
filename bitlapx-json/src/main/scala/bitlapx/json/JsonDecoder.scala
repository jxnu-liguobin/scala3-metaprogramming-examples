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

import bitlapx.common.Bitlapx.{ summonInlineOpt, typeName }
import adts.*
import bitlapx.common.Bitlapx
import bitlapx.json
import magnolia1.{ AutoDerivation, CaseClass, SealedTrait }

import scala.deriving.Mirror
import scala.reflect.{ classTag, ClassTag }
import scala.collection.immutable.ListMap
import scala.compiletime.*
import scala.deriving.*
import scala.quoted.{ Quotes, Type }
import scala.util.Right

trait JsonDecoder[A]:

  def decode(json: Json): Result[A]

end JsonDecoder

object JsonDecoder:

  given BigDecimalDecoder: JsonDecoder[BigDecimal] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value)
      case n               => fail[BigDecimal](n)

  given JavaBigDecimalDecoder: JsonDecoder[java.math.BigDecimal] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value)
      case n               => fail[java.math.BigDecimal](n)

  given SymbolDecoder: JsonDecoder[Symbol] = (json: Json) =>
    json match
      case Json.Str(value) => Right(Symbol(value))
      case n               => fail[Symbol](n)

  given StringDecoder: JsonDecoder[String] = (json: Json) =>
    json match
      case Json.Str(value: String) => Right(value)
      case n                       => fail[String](n)

  given BoolDecoder: JsonDecoder[Boolean] = (json: Json) =>
    json match
      case Json.Bool(value) => Right(value)
      case n                => fail[Boolean](n)

  given ShortDecoder: JsonDecoder[Short] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value.shortValue())
      case n               => fail[Short](n)

  given LongDecoder: JsonDecoder[Long] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value.longValue())
      case n               => fail[Long](n)

  given ByteDecoder: JsonDecoder[Byte] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value.byteValue())
      case n               => fail[Byte](n)

  given IntDecoder: JsonDecoder[Int] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value.intValue())
      case n               => fail[Int](n)

  given FloatDecoder: JsonDecoder[Float] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value.floatValue())
      case n               => fail[Float](n)

  given DoubleDecoder: JsonDecoder[Double] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value.doubleValue())
      case n               => fail[Double](n)

  given ArrDecoder[V](using js: JsonDecoder[V], ct: ClassTag[V]): JsonDecoder[List[V]] = (json: Json) =>
    json match
      case Json.Arr(list: List[Json]) =>
        val init: Result[List[V]] = Right(List.empty)
        list
          .foldRight(init) { case (json, resultTs) =>
            for
              t  <- js.decode(json)
              ts <- resultTs
            yield t :: ts
          }
      case n => fail[List[V]](n)

  inline given derived[V](using m: Mirror.Of[V], ct: ClassTag[V]): JsonDecoder[V] = (json: Json) =>
    json match
      case Json.Obj(map) =>
        inline m match {
          case s: Mirror.SumOf[V] =>
            Left(s"Not support type: ${classTag[V].runtimeClass.getSimpleName}")
          case p: Mirror.ProductOf[V] =>
            fromListMap[m.MirroredElemTypes, m.MirroredElemLabels](map, 0).map(t => p.fromProduct(t.asInstanceOf))
        }
      case o => fail[V](o)

  private inline def fail[V](js: => Json)(using ct: ClassTag[V]) = {
    val name = ct.runtimeClass.getSimpleName
    Left(s"Expected: $name, got: $js")
  }

  private inline def fromListMap[T, L](map: ListMap[String, Json], i: Int): Result[Tuple] =
    inline erasedValue[(T, L)] match
      case _: (EmptyTuple, EmptyTuple) => Right(Tuple())
      case _: (t *: ts, l *: ls) =>
        val js    = summonInline[JsonDecoder[t]]
        val label = constValue[l].asInstanceOf[String]

        for {
          j <- map.get(label).toRight(s"No such element: $label")
          h <- js.decode(j)
          t <- fromListMap[ts, ls](map, i + 1)
        } yield h *: t
