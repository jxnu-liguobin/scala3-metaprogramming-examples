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
import magnolia1.*

import scala.collection.immutable.*
import scala.collection.mutable
import scala.deriving.Mirror
import scala.reflect.ClassTag

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/24
 */
object DeriveJsonCodec:
  inline def gen[A](using mirror: Mirror.Of[A], ct: ClassTag[A]) = {
    val encoder = DeriveJsonEncoder.gen[A]
    val decoder = DeriveJsonDecoder.gen[A]
    JsonCodec(encoder, decoder)
  }

object DeriveJsonEncoder extends AutoDerivation[JsonEncoder]:
  self =>

  override def join[A](ctx: CaseClass[Typeclass, A]): Typeclass[A] =
    if (ctx.params.isEmpty) { (_: A) =>
      Json.Obj(ListMap.empty)
    } else
      (a: A) =>
        Json.Obj(
          ctx.params
            .foldLeft[ListMap[String, Json]](ListMap.empty) { case (chunk, param) =>
              val name  = param.label
              val value = param.typeclass.encode(param.deref(a))
              if (value == Json.Null) chunk
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

object DeriveJsonDecoder extends AutoDerivation[JsonDecoder]:
  self =>

  override def split[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] = (json: Json) =>
    val names: Array[String] = IArray.genericWrapArray(ctx.subtypes.map(p => p.typeInfo.short)).toArray
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
      IArray.genericWrapArray(ctx.params.map(_.label)).toArray
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

  inline def gen[A](using mirror: Mirror.Of[A], ct: ClassTag[A]) = self.derived[A]
