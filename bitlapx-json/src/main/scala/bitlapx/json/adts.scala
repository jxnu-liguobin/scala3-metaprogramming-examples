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

import bitlapx.common.Bitlapx.typeName
import bitlapx.json.adts.Json.*

import scala.collection.immutable.ListMap

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/23
 */
object adts {
  type Error     = String
  type Result[V] = Either[Error, V]

  sealed abstract class Json {
    self =>
    def prettyPrint: String = this match
      case Str(value)  => value
      case Num(value)  => value.toString
      case Bool(value) => value.toString
      case Arr(list)   => s"[${list.map(_.prettyPrint).mkString(", ")}]"
      case Obj(map)    => s"{${map.map(this.toObjectPair).mkString(", ")}}"
      case Json.Null   => "null"

    private def toObjectPair(k: String, v: Json): String = s""""$k": ${v.prettyPrint}"""

  }

  object Json {
    final case class Str(value: String) extends Json

    final case class Num(value: java.math.BigDecimal) extends Json

    object Num {
      def apply(value: Byte): Num = Num(BigDecimal(value.toInt).bigDecimal)

      def apply(value: Short): Num = Num(BigDecimal(value.toInt).bigDecimal)

      def apply(value: Int): Num = Num(BigDecimal(value).bigDecimal)

      def apply(value: Long): Num = Num(BigDecimal(value).bigDecimal)

      def apply(value: BigDecimal): Num = Num(value.bigDecimal)

      def apply(value: Float): Num = Num(BigDecimal(value).bigDecimal)

      def apply(value: Double): Num = Num(BigDecimal(value).bigDecimal)
    }

    final case class Bool(value: Boolean) extends Json

    final case class Arr(list: List[Json]) extends Json

    final case class Obj(map: ListMap[String, Json]) extends Json

    case object Null extends Json
  }

}
