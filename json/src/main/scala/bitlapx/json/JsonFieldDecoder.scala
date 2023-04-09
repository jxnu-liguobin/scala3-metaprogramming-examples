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

/** @author
 *    梦境迷离
 *  @version 1.0,2023/3/4
 */
trait JsonFieldDecoder[+A]:
  self =>

  final def map[B](f: A => B): JsonFieldDecoder[B] =
    (in: String) => f(self.unsafeDecodeField(in))

  final def mapOrFail[B](f: A => Either[String, B]): JsonFieldDecoder[B] =
    (in: String) =>
      f(self.unsafeDecodeField(in)) match {
        case Left(_)  => throw new Exception(s"Invalid json key: $in")
        case Right(b) => b
      }

  def unsafeDecodeField(in: String): A
end JsonFieldDecoder

object JsonFieldDecoder:
  def apply[A](using a: JsonFieldDecoder[A]): JsonFieldDecoder[A] = a

  given string: JsonFieldDecoder[String] = (in: String) => in

  given int: JsonFieldDecoder[Int] =
    JsonFieldDecoder[String].mapOrFail { str =>
      try
        Right(str.toInt)
      catch {
        case n: NumberFormatException => Left(s"Invalid Int: '$str': $n")
      }
    }

  given long: JsonFieldDecoder[Long] =
    JsonFieldDecoder[String].mapOrFail { str =>
      try
        Right(str.toLong)
      catch {
        case n: NumberFormatException => Left(s"Invalid Long: '$str': $n")
      }
    }
