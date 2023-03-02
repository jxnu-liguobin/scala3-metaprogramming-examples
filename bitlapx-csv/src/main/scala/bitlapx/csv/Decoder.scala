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

import magnolia1.*

import scala.compiletime.{ constValue, erasedValue }
import scala.deriving.Mirror
import bitlapx.common.SimpleUtils.*
import bitlapx.csv.ast.*

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/22
 */
trait Decoder[T]:
  def decode(str: String): ParseResult[T]
end Decoder

object Decoder extends AutoDerivation[Decoder] {

  def apply[T: Decoder] = summon[Decoder[T]]

  def join[T](ctx: CaseClass[Decoder.Typeclass, T]): Decoder[T] = value =>
    val parseResult = ctx.params
      .foldLeft[Option[ParseResult[Seq[Any]]]](None) { (state, p) =>
        state match {
          // first param
          case None =>
            Some(
              p.typeclass
                .decode(value)
                .map { x =>
                  ParseSuccess(x.remainder, Seq(x.value))
                }
                .left
                .map {
                  case e: ParseError.InvalidValue =>
                    e.copy(label = p.label)
                  case e: ParseError.NoValue =>
                    e.copy(label = p.label)
                }
            )
          // nothing left
          case Some(Right(previous)) if previous.remainder.isEmpty =>
            Some(Left(ParseError.NoValue(p.label)))
          // next params
          case Some(Right(previous)) =>
            Some(
              p.typeclass
                .decode(previous.remainder.mkString(","))
                .map { x =>
                  ParseSuccess(x.remainder, previous.value.appended(x.value))
                }
                .left
                .map {
                  case e: ParseError.InvalidValue =>
                    e.copy(label = p.label)
                  case e: ParseError.NoValue =>
                    e.copy(label = p.label)
                }
            )
          case Some(l @ Left(_)) =>
            Some(l)
        }
      }
      .fold(fail(ParseError.NoValue()))(identity)
    parseResult.map(x => ParseSuccess(x.remainder, ctx.rawConstruct(x.value)))

  def split[T](ctx: SealedTrait[Decoder, T]): Decoder[T] = value => ???

  given optionCodec[T: Decoder]: Decoder[Option[T]] = s =>
    s.split(",").toList match {
      case Nil => fail(ParseError.NoValue())
      case head :: tail =>
        if head.trim == "" then success(None, tail)
        else
          val dec = summon[Decoder[T]]
          dec
            .decode(head.trim)
            .map { x =>
              ParseSuccess(tail, Some(x.value))
            }
    }

  given Decoder[String] = s =>
    s.split(",").toList match {
      case Nil => fail(ParseError.NoValue())
      case head :: tail =>
        if head.trim == "" then fail(ParseError.NoValue())
        else success(head.trim, tail)
    }

  given Decoder[Int] = s =>
    s.split(",").toList match {
      case Nil => fail(ParseError.NoValue())
      case head :: tail =>
        if head.trim == "" then fail(ParseError.NoValue())
        head.trim.toIntOption
          .fold(fail(ParseError.InvalidValue(head, runtimeName[Int])))(int => success(int, tail))
    }

  given Decoder[Short] = s =>
    s.split(",").toList match {
      case Nil => fail(ParseError.NoValue())
      case head :: tail =>
        if head.trim == "" then fail(ParseError.NoValue())
        head.trim.toShortOption
          .fold(fail(ParseError.InvalidValue(head, runtimeName[Short])))(s => success(s, tail))
    }

  given Decoder[Float] = s =>
    s.split(",").toList match {
      case Nil => fail(ParseError.NoValue())
      case head :: tail =>
        if head.trim == "" then fail(ParseError.NoValue())
        head.trim.toFloatOption
          .fold(fail(ParseError.InvalidValue(head, runtimeName[Float])))(f => success(f, tail))
    }

  given Decoder[Double] = s =>
    s.split(",").toList match {
      case Nil => fail(ParseError.NoValue())
      case head :: tail =>
        if head.trim == "" then fail(ParseError.NoValue())
        head.trim.toDoubleOption
          .fold(fail(ParseError.InvalidValue(head, runtimeName[Double])))(d => success(d, tail))
    }

  given Decoder[Char] = s =>
    s.split(",").toList match {
      case Nil => fail(ParseError.NoValue())
      case head :: tail =>
        if head.trim == "" then fail(ParseError.NoValue())
        head.trim.toIntOption
          .map(_.toChar)
          .fold(fail(ParseError.InvalidValue(head, runtimeName[Char])))(c => success(c, tail))
    }

  given Decoder[Byte] = s =>
    s.split(",").toList match {
      case Nil => fail(ParseError.NoValue())
      case head :: tail =>
        if head.trim == "" then fail(ParseError.NoValue())
        head.trim.toByteOption
          .fold(fail(ParseError.InvalidValue(head, runtimeName[Byte])))(b => success(b, tail))
    }

  given Decoder[Boolean] = s =>
    s.split(",").toList match {
      case Nil => fail(ParseError.NoValue())
      case head :: tail =>
        if head.trim == "" then fail(ParseError.NoValue())
        else
          head.trim.toBooleanOption
            .fold(fail(ParseError.InvalidValue(head, runtimeName[Boolean])))(bool => success(bool, tail))
    }

  given Decoder[Long] = s =>
    s.split(",").toList match {
      case Nil => fail(ParseError.NoValue())
      case head :: tail =>
        if head.trim == "" then fail(ParseError.NoValue())
        else
          head.trim.toLongOption
            .fold(fail(ParseError.InvalidValue(head, runtimeName[Long])))(l => success(l, tail))
    }
}
