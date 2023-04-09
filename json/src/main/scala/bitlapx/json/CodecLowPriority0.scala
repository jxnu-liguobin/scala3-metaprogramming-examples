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

import scala.collection.immutable

/** @author
 *    梦境迷离
 *  @version 1.0,2023/3/4
 */
private[json] trait CodecLowPriority0 extends CodecLowPriority1:
  given hashSet[A: JsonEncoder: JsonDecoder]: JsonCodec[immutable.HashSet[A]] =
    JsonCodec(JsonEncoder.hashSet[A], JsonDecoder.hashSet[A])

  given hashMap[K: JsonFieldEncoder: JsonFieldDecoder, V: JsonEncoder: JsonDecoder]
    : JsonCodec[immutable.HashMap[K, V]] =
    JsonCodec(JsonEncoder.hashMap[K, V], JsonDecoder.hashMap[K, V])

end CodecLowPriority0

private[json] trait CodecLowPriority1 extends CodecLowPriority2:

  given seq[A: JsonEncoder: JsonDecoder]: JsonCodec[immutable.Seq[A]] =
    JsonCodec(JsonEncoder.seq[A], JsonDecoder.seq[A])

  given list[A: JsonEncoder: JsonDecoder]: JsonCodec[immutable.List[A]] =
    JsonCodec(JsonEncoder.list[A], JsonDecoder.list[A])

  given vector[A: JsonEncoder: JsonDecoder]: JsonCodec[immutable.Vector[A]] =
    JsonCodec(JsonEncoder.vector[A], JsonDecoder.vector[A])

  given set[A: JsonEncoder: JsonDecoder]: JsonCodec[immutable.Set[A]] =
    JsonCodec(JsonEncoder.set[A], JsonDecoder.set[A])

  given map[K: JsonFieldEncoder: JsonFieldDecoder, V: JsonEncoder: JsonDecoder]: JsonCodec[Map[K, V]] =
    JsonCodec(JsonEncoder.map[K, V], JsonDecoder.map[K, V])

  given sortedMap[
    K: JsonFieldEncoder: JsonFieldDecoder: Ordering,
    V: JsonEncoder: JsonDecoder
  ]: JsonCodec[collection.SortedMap[K, V]] =
    JsonCodec(JsonEncoder.sortedMap[K, V], JsonDecoder.sortedMap[K, V])

  given sortedSet[A: Ordering: JsonEncoder: JsonDecoder]: JsonCodec[immutable.SortedSet[A]] =
    JsonCodec(JsonEncoder.sortedSet[A], JsonDecoder.sortedSet[A])

end CodecLowPriority1

private[json] trait CodecLowPriority2:
  given iterable[A: JsonEncoder: JsonDecoder]: JsonCodec[Iterable[A]] =
    JsonCodec(JsonEncoder.iterable[A, Iterable], JsonDecoder.builder(Iterable.newBuilder[A]))
