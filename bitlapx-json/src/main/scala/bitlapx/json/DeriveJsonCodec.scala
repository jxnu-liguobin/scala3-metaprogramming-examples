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

import bitlapx.json.adts.*
import magnolia1.{ AutoDerivation, CaseClass, SealedTrait }

import scala.collection.immutable.ListMap
import scala.deriving.Mirror
import scala.reflect.ClassTag

object DeriveJsonCodec:
  inline def gen[A](using mirror: Mirror.Of[A], ct: ClassTag[A]) = {
    val encoder = DeriveJsonEncoder.gen[A]
    val decoder = DeriveJsonDecoder.gen[A]
    JsonCodec(encoder, decoder)
  }

object DeriveJsonEncoder extends AutoDerivation[JsonEncoder]:
  self =>

  override def join[A](ctx: CaseClass[Typeclass, A]): Typeclass[A] = (a: A) =>
    Json.Obj(
      ctx.params
        .foldLeft[ListMap[String, Json]](ListMap.empty) { case (chunk, param) =>
          val name  = param.label
          val value = param.typeclass.encode(param.deref(a))
          if (value == Json.Null) chunk
          else ListMap(name -> value) ++ chunk
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

object DeriveJsonDecoder:
  self =>

// TODO
//  override def split[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] = new JsonDecoder[T]:
//    override def decode(js: Json): Result[T] = ???
//
//  override def join[T](ctx: CaseClass[Typeclass, T]): Typeclass[T] = new JsonDecoder[T]:
//    override def decode(json: Json): Result[T] = ???

  inline def gen[A](using mirror: Mirror.Of[A], ct: ClassTag[A]) = JsonDecoder.derived[A]
