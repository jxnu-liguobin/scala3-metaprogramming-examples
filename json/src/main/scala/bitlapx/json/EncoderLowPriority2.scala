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

import bitlapx.json.ast.Json

import scala.collection.immutable
import scala.collection.immutable.ListMap
import scala.util.Right

/** @author
 *    梦境迷离
 *  @version 1.0,2023/3/4
 */
private[json] trait EncoderLowPriority2:

  given iterable[A, T[X] <: Iterable[X]](using jsonEncoder: JsonEncoder[A]): JsonEncoder[T[A]] = (as: T[A]) =>
    Json.Arr(
      as.map(jsonEncoder.encode)
        .foldLeft[List[Json]](List.empty) { (s, i) =>
          s ::: List(i)
        }
    )

  def keyValueIterable[K, A, T[X, Y] <: Iterable[(X, Y)]](using K: JsonFieldEncoder[K], A: JsonEncoder[A]) =
    new JsonEncoder[T[K, A]]:
      override def encode(kvs: T[K, A]): Json =
        Json.Obj(
          kvs
            .foldLeft[ListMap[String, Json]](ListMap.empty) { case (s, (k, v)) =>
              val key   = K.unsafeEncodeField(k)
              val value = A.encode(v)
              if value == Json.Null then s else s ++ ListMap(key -> value)
            }
        )
