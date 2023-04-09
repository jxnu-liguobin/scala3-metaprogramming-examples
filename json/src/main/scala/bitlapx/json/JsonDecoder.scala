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
import bitlapx.json.annotation.*
import magnolia1.*

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.deriving.Mirror
import scala.reflect.*
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

object JsonDecoder extends DecoderLowPriority1 with AutoDerivation[JsonDecoder]:
  self =>

  override def split[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] = (json: Json) =>
    lazy val names: Array[String] = IArray.genericWrapArray(ctx.subtypes.map(p => p.typeInfo.short)).toArray
    lazy val tcs: Array[JsonDecoder[Any]] =
      IArray.genericWrapArray(ctx.subtypes.map(_.typeclass)).toArray.asInstanceOf[Array[JsonDecoder[Any]]]
    lazy val namesMap: Map[String, Int] =
      names.zipWithIndex.toMap
    json match
      case Json.Obj(chunk) if chunk.size == 1 =>
        val (key, inner) = chunk.head
        namesMap.get(key) match {
          case Some(idx) => tcs(idx).decode(inner).map(_.asInstanceOf[T])
          case None      => Left("Invalid disambiguator")
        }
      case Json.Obj(_) => Left("Not an object with a single field")
      case _           => Left("Not an object")

  override def join[T](ctx: CaseClass[Typeclass, T]): Typeclass[T] = (json: Json) =>
    val names: Array[String] =
      IArray
        .genericWrapArray(ctx.params.map { p =>
          p.annotations.collectFirst { case jsonField(name) => name }
            .getOrElse(p.label)
        })
        .toArray
    val len                             = names.length
    lazy val namesMap: Map[String, Int] = names.zipWithIndex.toMap
    val tcs: Array[JsonDecoder[Any]] =
      IArray.genericWrapArray(ctx.params.map(_.typeclass)).toArray.asInstanceOf[Array[JsonDecoder[Any]]]
    val failures = new mutable.LinkedHashSet[String]
    json match
      case Json.Obj(fields) =>
        val ps: Array[Any] = Array.ofDim(len)
        for ((key, value) <- fields)
          namesMap.get(key) match {
            case Some(field) =>
              ps(field) = tcs(field).decode(value) match {
                case Left(error)  => failures += error; null
                case Right(value) => value
              }
            case None => Left("Invalid extra field")
          }

        Right(ctx.rawConstruct(ArraySeq.unsafeWrapArray(ps)))
      case _ => Left("Not an object")

  inline def gen[A](using mirror: Mirror.Of[A]) = self.derived[A]

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
          } else Left(s"Invalid either value: $json")
        case _ => Left(s"Invalid either value: $json")

  private inline def fail[V: ClassTag](js: => Json) = {
    val name = classTag[V].runtimeClass.getSimpleName
    Left(s"Expected: $name, got: $js")
  }
