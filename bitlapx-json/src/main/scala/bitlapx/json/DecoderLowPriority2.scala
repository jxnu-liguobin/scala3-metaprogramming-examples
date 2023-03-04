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

import scala.Seq
import scala.collection.{immutable, mutable}
import scala.collection.immutable.ListMap
import scala.reflect.ClassTag

/** @author
 *    梦境迷离
 *  @version 1.0,2023/3/4
 */
private[json] trait DecoderLowPriority2 {

  def builder[A, T[_]](builder: mutable.Builder[A, T[A]])(using jsonDecoder: JsonDecoder[A]): JsonDecoder[T[A]] =
    (json: Json) =>
      json match
        case Json.Arr(elements) =>
          elements.map(jsonDecoder.decode).foreach(i => i.map(builder.addOne))
          Right(builder.result())
        case _ => Left(s"Not an iterable: $json")

  def keyValueBuilder[K, A, T[X, Y] <: Iterable[(X, Y)]](
    builder: mutable.Builder[(K, A), T[K, A]]
  )(using K: JsonFieldDecoder[K], A: JsonDecoder[A]) =
    new JsonDecoder[T[K, A]]:
      override def decode(json: Json): Result[T[K, A]] =
        json match
          case Json.Obj(listMap) =>
            val listMapE = listMap
              .foldLeft[Result[ListMap[K, A]]](Right(ListMap.empty[K, A])) { (s, item) =>
                val key = K.unsafeDecodeField(item._1)
                val v   = A.decode(item._2).flatMap(value => Right(key -> value))
                s.flatMap(r => v.map(vv => ListMap(vv) ++ r))
              }
            listMapE.map(lm => builder.addAll(lm).result())
          case _ => Left(s"Not a map: $json")

}
