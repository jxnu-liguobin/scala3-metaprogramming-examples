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

import scala.collection.{Iterable, immutable, mutable}
import scala.collection.immutable.*
import scala.reflect.ClassTag

/** @author
 *    梦境迷离
 *  @version 1.0,2023/3/4
 */
private[json] trait DecoderLowPriority1 extends DecoderLowPriority2 {

  given array[A](using jsonDecoder: JsonDecoder[A], classTag: ClassTag[A]): JsonDecoder[Array[A]] =
    (json: Json) =>
      json match
        case Json.Arr(elements) =>
          elements.foldLeft[Result[Array[A]]](Right(Array.empty[A])) { (s, item) =>
            jsonDecoder.decode(item).flatMap { a =>
              s.map(as => Array(a) ++ as)
            }
          }
        case _ => Left(s"Not an array: $json")

  given seq[A: JsonDecoder]: JsonDecoder[immutable.Seq[A]] = builder[A, Seq](Seq.newBuilder[A])

  given indexedSeq[A: JsonDecoder]: JsonDecoder[IndexedSeq[A]] = builder[A, IndexedSeq](IndexedSeq.newBuilder[A])

  given linearSeq[A: JsonDecoder]: JsonDecoder[LinearSeq[A]] = builder[A, LinearSeq](LinearSeq.newBuilder[A])

  given listSet[A: JsonDecoder]: JsonDecoder[immutable.ListSet[A]] = builder[A, immutable.ListSet](immutable.ListSet.newBuilder[A])

  given treeSet[A: JsonDecoder:Ordering]: JsonDecoder[immutable.TreeSet[A]] = builder[A, immutable.TreeSet](immutable.TreeSet.newBuilder[A])

  given list[A: JsonDecoder]: JsonDecoder[List[A]] = builder[A, List](List.newBuilder[A])

  given vector[A: JsonDecoder]: JsonDecoder[Vector[A]] = builder[A, Vector](Vector.newBuilder[A])

  given set[A: JsonDecoder]: JsonDecoder[Set[A]] = builder[A, Set](Set.newBuilder[A])

  given hashSet[A: JsonDecoder]: JsonDecoder[immutable.HashSet[A]] = builder[A, immutable.HashSet](immutable.HashSet.newBuilder[A])

  given sortedSet[A: Ordering: JsonDecoder]: JsonDecoder[SortedSet[A]] =
    builder[A, SortedSet](SortedSet.newBuilder[A])

  given hashMap[K: JsonFieldDecoder, V: JsonDecoder]: JsonDecoder[immutable.HashMap[K, V]] =
    keyValueBuilder[K, V, immutable.HashMap](immutable.HashMap.newBuilder[K, V])

  given mutableHashMap[K: JsonFieldDecoder, V: JsonDecoder]: JsonDecoder[mutable.HashMap[K, V]] =
    keyValueBuilder[K, V, mutable.HashMap](mutable.HashMap.newBuilder[K, V])

  given map[K: JsonFieldDecoder, V: JsonDecoder]: JsonDecoder[Map[K, V]] =
    keyValueBuilder[K, V, Map](Map.newBuilder[K, V])

  given sortedMap[K: JsonFieldDecoder: Ordering, V: JsonDecoder]: JsonDecoder[SortedMap[K, V]] =
    keyValueBuilder[K, V, SortedMap](SortedMap.newBuilder[K, V])

}
