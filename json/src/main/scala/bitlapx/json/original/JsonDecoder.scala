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

import bitlapx.common.Syntax.recurse
import bitlapx.json.ast.*
import bitlapx.common.TypeInfo
import bitlapx.json
import bitlapx.json.{ DecoderLowPriority1, JsonDecoder }
import bitlapx.json.annotation.jsonField

import scala.deriving.Mirror
import scala.reflect.*
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*
import scala.util.Right

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/24
 */
object JsonDecoder extends DecoderLowPriority1:

  inline def derived[V](using m: Mirror.Of[V]): JsonDecoder[V] = (json: Json) =>
    json match
      case Json.Obj(map) =>
        inline m match {
          case s: Mirror.SumOf[V] =>
            if (map.keys.size > 1) Left(s"""Invalid json obj: ${json.asJsonString}, cannot be a sum type""")
            else
              val subTypes = recurse[s.MirroredElemTypes, JsonDecoder].toMap
              val subType  = map.keys.toList.intersect(subTypes.keys.toList)
              if (subType.isEmpty)
                Left(
                  s"""Invalid json obj: ${json.asJsonString}, cannot be a sum type, available subTypes: ${subTypes.keys
                      .mkString(",")}"""
                )
              else
                subType.headOption
                  .map(st => subTypes(st).decode(map(st)))
                  .getOrElse(
                    Left(s"""unknown error""")
                  )
                  .asInstanceOf[Result[V]]

          case p: Mirror.ProductOf[V] =>
            val pans: Map[String, List[Any]] = TypeInfo.paramAnns[V].to(Map)
            fromListMap[m.MirroredElemTypes, m.MirroredElemLabels](map, 0)(pans).map(t => p.fromProduct(t.asInstanceOf))
        }
      case o => Left(s"""Invalid json obj: ${o.asJsonString}""")

  private inline def fromListMap[T, L](map: ListMap[String, Json], i: Int)(
    pans: Map[String, List[Any]]
  ): Result[Tuple] =
    inline erasedValue[(T, L)] match
      case _: (EmptyTuple, EmptyTuple) => Right(Tuple())
      case _: (t *: ts, l *: ls) =>
        val js    = summonInline[JsonDecoder[t]]
        val label = constValue[l].asInstanceOf[String]
        val name: String =
          pans.get(label).fold(label)(as => as.collectFirst { case jsonField(name) => name }.getOrElse(label))
        for {
          j <- map.get(name).toRight(s"No such element: $name")
          h <- js.decode(j)
          t <- fromListMap[ts, ls](map, i + 1)(pans)
        } yield h *: t
