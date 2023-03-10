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

package bitlapx.json.original

import bitlapx.json.ast.*
import bitlapx.common.MacroTools.{ isEnum, * }
import bitlapx.common.{ MacroTools, TypeInfo }
import bitlapx.common.TypeInfo.typeInfo
import bitlapx.json.{ DecoderLowPriority1, EncoderLowPriority1, JsonDecoder, JsonEncoder }
import bitlapx.json.annotation.*

import scala.collection.immutable.ListMap
import scala.compiletime.*
import scala.deriving.*
import scala.deriving.Mirror
import scala.quoted.*
import scala.reflect.*

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/24
 */
object JsonEncoder extends EncoderLowPriority1:

  inline def derived[V](using m: Mirror.Of[V]): JsonEncoder[V] = (v: V) => {
    val pans: Map[String, List[Any]] = TypeInfo.paramAnns[V].to(Map)
    inline m match {
      case s: Mirror.SumOf[V] =>
        val subTypes = recurse[s.MirroredElemTypes, JsonEncoder]
        val idx      = s.ordinal(v)
        val encoder  = subTypes(idx)
        Json.Obj(
          ListMap(
            encoder._1 -> encoder._2.encode(v)
          )
        )
      case _: Mirror.ProductOf[V] =>
        Json.Obj(
          toListMap[m.MirroredElemTypes, m.MirroredElemLabels, V](v, 0)(pans)
        )
    }
  }

  private inline def toListMap[T, L, V](v: V, i: Int)(pans: Map[String, List[Any]]): ListMap[String, Json] =
    inline erasedValue[(T, L)] match
      case _: (EmptyTuple, EmptyTuple) => ListMap.empty
      case _: (t *: ts, l *: ls) =>
        val js      = summonInline[JsonEncoder[t]]
        val label   = constValue[l].asInstanceOf[String]
        val value   = js.encode(productElement[t](v, i))
        val exclude = pans.get(label).fold(false)(as => as.collectFirst { case _: jsonExclude => () }.isDefined)
        val name: String =
          pans.get(label).fold(label)(as => as.collectFirst { case jsonField(name) => name }.getOrElse(label))
        if exclude then toListMap[ts, ls, V](v, i + 1)(pans)
        else ListMap(name -> value) ++ toListMap[ts, ls, V](v, i + 1)(pans)
