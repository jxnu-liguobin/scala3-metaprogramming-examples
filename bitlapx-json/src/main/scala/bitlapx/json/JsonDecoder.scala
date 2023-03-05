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
import bitlapx.json

import scala.deriving.Mirror
import scala.reflect.*
import scala.collection.immutable.ListMap
import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*
import scala.util.Right

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/24
 */
trait JsonDecoder[A]:

  def decode(json: Json): Result[A]

end JsonDecoder

object JsonDecoder extends DecoderLowPriority1:

  def apply[A](using a: JsonDecoder[A]): JsonDecoder[A] = a

  given bigDecimalDecoder: JsonDecoder[BigDecimal] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value)
      case n               => fail[BigDecimal](n)

  given javaBigDecimalDecoder: JsonDecoder[java.math.BigDecimal] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value)
      case n               => fail[java.math.BigDecimal](n)

  given symbolDecoder: JsonDecoder[Symbol] = (json: Json) =>
    json match
      case Json.Str(value) => Right(Symbol(value))
      case n               => fail[Symbol](n)

  given stringDecoder: JsonDecoder[String] = (json: Json) =>
    json match
      case Json.Str(value: String) => Right(value)
      case n                       => fail[String](n)

  given boolDecoder: JsonDecoder[Boolean] = (json: Json) =>
    json match
      case Json.Bool(value) => Right(value)
      case n                => fail[Boolean](n)

  given shortDecoder: JsonDecoder[Short] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value.shortValue())
      case n               => fail[Short](n)

  given longDecoder: JsonDecoder[Long] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value.longValue())
      case n               => fail[Long](n)

  given byteDecoder: JsonDecoder[Byte] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value.byteValue())
      case n               => fail[Byte](n)

  given intDecoder: JsonDecoder[Int] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value.intValue())
      case n               => fail[Int](n)

  given floatDecoder: JsonDecoder[Float] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value.floatValue())
      case n               => fail[Float](n)

  given doubleDecoder: JsonDecoder[Double] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value.doubleValue())
      case n               => fail[Double](n)

  given arrDecoder[V](using js: JsonDecoder[V], ct: ClassTag[V]): JsonDecoder[List[V]] = (json: Json) =>
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

  given option[A](using jsonDecoder: JsonDecoder[A]): JsonDecoder[Option[A]] = (json: Json) =>
    json match
      case Json.Null => Right(None)
      case _         => jsonDecoder.decode(json).map(Some.apply)

  given either[A, B](using jsonDecoderA: JsonDecoder[A], jsonDecoderB: JsonDecoder[B]): JsonDecoder[Either[A, B]] =
    (json: Json) =>
      json match
        case Json.Obj(list) =>
          if (list.contains("Left")) {
            jsonDecoderA.decode(list("Left")).map(Left(_))
          } else if (list.contains("Right")) {
            jsonDecoderB.decode(list("Right")).map(Right(_))
          } else Left(s"Not an either: $json")
        case _ => Left(s"Not an either: $json")

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
