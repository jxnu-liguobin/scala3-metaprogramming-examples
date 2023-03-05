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

import scala.collection.immutable.ListMap
import scala.collection.{ immutable, mutable }
import scala.util.Right

/** @author
 *    梦境迷离
 *  @version 1.0,2023/3/4
 */
private[json] trait EncoderLowPriority1 extends EncoderLowPriority2 {

  given array[A](using jsonEncoder: JsonEncoder[A]): JsonEncoder[Array[A]] = (as: Array[A]) =>
    Json.Arr(
      as.map(jsonEncoder.encode)
        .foldLeft[List[Json]](List.empty) { (s, i) =>
          List(i) ::: s
        }
    )

  given seq[A: JsonEncoder]: JsonEncoder[Seq[A]] = iterable[A, Seq]

  given indexedSeq[A: JsonEncoder]: JsonEncoder[IndexedSeq[A]] = iterable[A, IndexedSeq]

  given linearSeq[A: JsonEncoder]: JsonEncoder[immutable.LinearSeq[A]] = iterable[A, immutable.LinearSeq]

  given listSet[A: JsonEncoder]: JsonEncoder[immutable.ListSet[A]] = iterable[A, immutable.ListSet]

  given treeSet[A: JsonEncoder]: JsonEncoder[immutable.TreeSet[A]] = iterable[A, immutable.TreeSet]

  given list[A: JsonEncoder]: JsonEncoder[List[A]] = iterable[A, List]

  given vector[A: JsonEncoder]: JsonEncoder[Vector[A]] = iterable[A, Vector]

  given set[A: JsonEncoder]: JsonEncoder[Set[A]] = iterable[A, Set]

  given hashSet[A: JsonEncoder]: JsonEncoder[immutable.HashSet[A]] = iterable[A, immutable.HashSet]

  given sortedSet[A: Ordering: JsonEncoder]: JsonEncoder[immutable.SortedSet[A]] =
    iterable[A, immutable.SortedSet]

  given hashMap[K: JsonFieldEncoder, V: JsonEncoder]: JsonEncoder[immutable.HashMap[K, V]] =
    keyValueIterable[K, V, immutable.HashMap]

  given map[K: JsonFieldEncoder, V: JsonEncoder]: JsonEncoder[Map[K, V]] =
    keyValueIterable[K, V, Map]

  given mutableMap[K: JsonFieldEncoder, V: JsonEncoder]: JsonEncoder[mutable.Map[K, V]] =
    keyValueIterable[K, V, mutable.Map]

  given sortedMap[K: JsonFieldEncoder, V: JsonEncoder]: JsonEncoder[collection.SortedMap[K, V]] =
    keyValueIterable[K, V, collection.SortedMap]
}
