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

import scala.compiletime.*
import scala.deriving.*
import scala.deriving.Mirror
import scala.quoted.*

/** @author
 *    梦境迷离
 *  @version 1.0,2023/3/11
 */
trait ExtraEnumImplicits:

  inline given stringEnumEncoder[T <: scala.reflect.Enum](using m: Mirror.SumOf[T]): JsonEncoder[T] = {
    val elemInstances =
      summonAll[Tuple.Map[m.MirroredElemTypes, ValueOf]].productIterator.asInstanceOf[Iterator[ValueOf[T]]].map(_.value)
    val elemNames = summonAll[Tuple.Map[m.MirroredElemLabels, ValueOf]].productIterator
      .asInstanceOf[Iterator[ValueOf[String]]]
      .map(_.value)
    val mapping = (elemInstances zip elemNames).toMap
    (a: T) => JsonEncoder[String].encode(mapping.apply(a))
  }

  inline given stringEnumDecoder[T <: scala.reflect.Enum](using m: Mirror.SumOf[T]): JsonDecoder[T] = {
    val elemInstances =
      summonAll[Tuple.Map[m.MirroredElemTypes, ValueOf]].productIterator.asInstanceOf[Iterator[ValueOf[T]]].map(_.value)
    val elemNames = summonAll[Tuple.Map[m.MirroredElemLabels, ValueOf]].productIterator
      .asInstanceOf[Iterator[ValueOf[String]]]
      .map(_.value)
    val mapping = (elemNames zip elemInstances).toMap
    (json: Json) =>
      json match
        case Json.Str(value) => mapping.get(value).fold(Left(s"Name $value is invalid enum value"))(Right(_))
        case _               => Left(s"Invalid enum value: $json")
  }
