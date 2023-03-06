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
import bitlapx.common.TypeInfo
import bitlapx.json
import bitlapx.json.annotation.jsonField

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

  given JsonDecoder[BigDecimal] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value)
      case n               => fail[BigDecimal](n)

  given javaBigDecimalDecoder: JsonDecoder[java.math.BigDecimal] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value)
      case n               => fail[java.math.BigDecimal](n)

  given JsonDecoder[Symbol] = (json: Json) =>
    json match
      case Json.Str(value) => Right(Symbol(value))
      case n               => fail[Symbol](n)

  given JsonDecoder[String] = (json: Json) =>
    json match
      case Json.Str(value: String) => Right(value)
      case n                       => fail[String](n)

  given JsonDecoder[Boolean] = (json: Json) =>
    json match
      case Json.Bool(value) => Right(value)
      case n                => fail[Boolean](n)

  given JsonDecoder[Short] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value.shortValue())
      case n               => fail[Short](n)

  given JsonDecoder[Long] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value.longValue())
      case n               => fail[Long](n)

  given JsonDecoder[Byte] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value.byteValue())
      case n               => fail[Byte](n)

  given JsonDecoder[Int] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value.intValue())
      case n               => fail[Int](n)

  given JsonDecoder[Float] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value.floatValue())
      case n               => fail[Float](n)

  given JsonDecoder[Double] = (json: Json) =>
    json match
      case Json.Num(value) => Right(value.doubleValue())
      case n               => fail[Double](n)

  given [A](using js: JsonDecoder[A]): JsonDecoder[List[A]] = builder[A, List](List.newBuilder[A])

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

  inline given derived[V: ClassTag](using m: Mirror.Of[V]): JsonDecoder[V] = (json: Json) =>
    json match
      case Json.Obj(map) =>
        inline m match {
          case s: Mirror.SumOf[V] =>
            throw new Exception(s"Not support sum type")
          case p: Mirror.ProductOf[V] =>
            val pans: Map[String, List[Any]] = TypeInfo.paramAnns[V].to(Map)
            fromListMap[m.MirroredElemTypes, m.MirroredElemLabels](map, 0)(pans).map(t => p.fromProduct(t.asInstanceOf))
        }
      case o => fail[V](o)

  private inline def fail[V: ClassTag](js: => Json) = {
    val name = classTag[V].runtimeClass.getSimpleName
    Left(s"Expected: $name, got: $js")
  }

  private inline def fromListMap[T, L](map: ListMap[String, Json], i: Int)(pans: Map[String, List[Any]] ): Result[Tuple] =
    inline erasedValue[(T, L)] match
      case _: (EmptyTuple, EmptyTuple) => Right(Tuple())
      case _: (t *: ts, l *: ls) =>
        val js    = summonInline[JsonDecoder[t]]
        val label = constValue[l].asInstanceOf[String]
        val name:String = pans.get(label).fold(label)(as => as.collectFirst { case jsonField(name) => name}.getOrElse(label) )
        for {
          j <- map.get(name).toRight(s"No such element: $name")
          h <- js.decode(j)
          t <- fromListMap[ts, ls](map, i + 1)(pans)
        } yield h *: t
