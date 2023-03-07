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

package bitlapx.csv

import bitlapx.csv.ast.NotSupportTypeException
import bitlapx.csv.ast.ParseError.*
import magnolia1.*

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/22
 */
trait Encoder[T]:
  extension (value: T) def encode: List[String]
end Encoder

object Encoder extends AutoDerivation[Encoder] {

  def apply[T: Encoder] = summon[Encoder[T]]

  def join[T](ctx: CaseClass[Encoder.Typeclass, T]): Encoder[T] = value =>
    ctx.params.foldLeft[List[String]](List.empty) { (acc, p) =>
      acc ++ p.typeclass.encode(p.deref(value))
    }

  def split[T](ctx: SealedTrait[Encoder, T]): Encoder[T] = value =>
    ctx.choose(value) { sub =>
      throw NotSupportTypeException(value = value.toString, targetType = sub.typeInfo.short)
    }

  given Encoder[String] = s => List(s)

  given Encoder[Int] = s => List(s.toString)

  given Encoder[Boolean] = s => List(s.toString)

  given Encoder[Char] = s => List(s.toString)

  given Encoder[Float] = s => List(s.toString)

  given Encoder[Double] = s => List(s.toString)

  given Encoder[Short] = s => List(s.toString)

  given Encoder[Long] = s => List(s.toString)

  given Encoder[Byte] = s => List(s.toString)

  given optionCodec[T: Encoder]: Encoder[Option[T]] = s =>
    s match
      case Some(s) => s.encode
      case None    => List("")

}
