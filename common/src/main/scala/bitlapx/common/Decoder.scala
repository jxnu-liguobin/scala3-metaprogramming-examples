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

package bitlapx.common

import magnolia1.*

/** decoder for converting strings to other types providing good error messages
 */
trait Decoder[T]:
  def decode(str: String): Either[String, T]
end Decoder

object Decoder extends Derivation[Decoder]:

  given Decoder[String] = Right(_)

  given Decoder[Int] = k => k.toIntOption.toRight[String](s"illegal Int number: $k")

  given Decoder[BigDecimal] = k =>
    try Right(BigDecimal(k))
    catch case _: NumberFormatException => Left(s"illegal BigDecimal number: $k")

  given Decoder[Double] = k => k.toDoubleOption.toRight[String](s"illegal Double number: $k")

  given Decoder[Boolean] = k => k.toBooleanOption.toRight[String](s"illegal Boolean number: $k")

  def join[T](ctx: CaseClass[Decoder, T]): Decoder[T] = value =>
    val (_, values) = parse(value)

    ctx
      .constructEither(param => param.typeclass.decode(values(param.label)))
      .left
      .map(_.reduce(_ + "\n" + _))
  end join

  override def split[T](ctx: SealedTrait[Decoder, T]): Decoder[T] =
    param =>
      val (name, _) = parse(param)
      val subtype   = ctx.subtypes.find(_.typeInfo.full == name).get

      subtype.typeclass.decode(param)
  end split

  private def parse(value: String): (String, Map[String, String]) =
    val end  = value.indexOf('(')
    val name = value.substring(0, end)

    def parts(
      value: String,
      idx: Int = 0,
      depth: Int = 0,
      collected: List[String] = List("")
    ): List[String] =
      def plus(char: Char): List[String] =
        collected.head + char :: collected.tail

      if (idx == value.length) collected
      else
        value(idx) match
          case '(' =>
            parts(value, idx + 1, depth + 1, plus('('))
          case ')' =>
            if (depth == 1) plus(')')
            else parts(value, idx + 1, depth - 1, plus(')'))
          case ',' =>
            if (depth == 0) parts(value, idx + 1, depth, "" :: collected)
            else parts(value, idx + 1, depth, plus(','))
          case char =>
            parts(value, idx + 1, depth, plus(char))
    end parts

    def keyValue(str: String): (String, String) =
      val List(label, value) = str.split("=", 2).to(List)
      (label, value)
    end keyValue

    (
      name,
      parts(value.substring(end + 1, value.length - 1)).map(keyValue).to(Map)
    )
