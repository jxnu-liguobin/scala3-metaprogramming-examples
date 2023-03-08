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

import scala.deriving.Mirror

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/24
 */
object DeriveJsonCodec:
  inline def gen[A](using mirror: Mirror.Of[A]): JsonCodec[A] = {
    val encoder = JsonEncoder.gen[A]
    val decoder = JsonDecoder.gen[A]
    JsonCodec(encoder, decoder)
  }

  inline def originalGen[A](using mirror: Mirror.Of[A]): JsonCodec[A] = {
    val encoder = original.JsonEncoder.derived[A]
    val decoder = original.JsonDecoder.derived[A]
    new JsonCodec(encoder, decoder)
  }
