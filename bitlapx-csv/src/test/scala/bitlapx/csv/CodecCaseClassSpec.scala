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

import bitlapx.csv.ast.*
import bitlapx.csv.syntax.*
import bitlapx.csv.Encoder.*

class CodecCaseClassSpec extends munit.FunSuite {

  final case class SimpleDimension(id: Long, key: String) derives Decoder

  test("valid csv") {
    final case class Dimension(id: Long, key: String, valid: Boolean, name: Option[String]) derives Decoder, Encoder
    val dimString = "233,key,false, name"
    val obj       = Dimension(233, "key", false, Option("name"))
    assertEquals(
      dimString.parse[Dimension],
      Right(ParseSuccess(List.empty, obj))
    )

    assertEquals(obj.encode, "233,key,false,name".split(",").toList)

  }

  test("nested valid csv") {
    final case class Attribute(id: Long, key: String)
    final case class Dimension(id: Long, key: String, valid: Boolean, att: Attribute)
    final case class Metric(id: Long, key: String, dim: Dimension) derives Decoder, Encoder

    val metricString = "234,mkey,233,dkey,false,235,akey"
    val metric = Metric(
      234,
      "mkey",
      Dimension(233, "dkey", false, Attribute(235, "akey"))
    )
    assertEquals(
      metricString.parse[Metric],
      Right(ParseSuccess(List.empty, metric))
    )

    assertEquals(metric.encode, metricString.split(",").toList)
  }

  test("with optional string field 1") {
    final case class Dimension(att: Option[String], id: Long, key: String, valid: Boolean)
    final case class Metric(id: Long, key: String, dim: Dimension) derives Decoder, Encoder
    val metricString = "234,mkey, , 233,dkey,false"
    val metric = Metric(
      234,
      "mkey",
      Dimension(None, 233, "dkey", false)
    )
    assertEquals(
      metricString.parse[Metric],
      Right(ParseSuccess(List.empty, metric))
    )

    assertEquals(metric.encode, List("234", "mkey", "", "233", "dkey", "false"))
  }

  test("with optional string field 2") {
    final case class Dimension(id: Long, key: String, valid: Boolean, att: Option[String])
    final case class Metric(id: Long, key: String, dim: Dimension) derives Decoder, Encoder
    val metricString = "234,mkey, 233,dkey,false,att"
    val metric = Metric(
      234,
      "mkey",
      Dimension(233, "dkey", false, Option("att"))
    )
    assertEquals(
      metricString.parse[Metric],
      Right(ParseSuccess(List.empty, metric))
    )

    assertEquals(metric.encode, metricString.split(",").map(_.trim).toList)
  }

  test("with optional string field 3") {
    final case class Dimension(id: Long, key: String, att: Option[String], valid: Boolean)
    final case class Metric(id: Long, key: String, dim: Dimension) derives Decoder, Encoder
    val metricString = "234, mkey, 233,dkey,att,false"
    val metric = Metric(
      234,
      "mkey",
      Dimension(233, "dkey", Option("att"), false)
    )
    assertEquals(
      metricString.parse[Metric],
      Right(ParseSuccess(List.empty, metric))
    )

    assertEquals(metric.encode, metricString.split(",").map(_.trim).toList)
  }

  test("with optional string field 4") {
    final case class Dimension(att: Option[String], id: Long, key: String, valid: Boolean)
    final case class Metric(id: Long, key: String, dim: Dimension) derives Decoder, Encoder
    val metricString = "234, mkey,att, 233,dkey,false"
    val metric = Metric(
      234,
      "mkey",
      Dimension(Option("att"), 233, "dkey", false)
    )
    assertEquals(
      metricString.parse[Metric],
      Right(ParseSuccess(List.empty, metric))
    )

    assertEquals(metric.encode, metricString.split(",").map(_.trim).toList)
  }

  test("ParseError msg") {
    val str = "bitlap"
    assertEquals(
      str.parse[SimpleDimension],
      Left(ParseError.InvalidValue("bitlap", "long", "id"))
    )
  }

  test("invalid param type") {
    val person = "abc,233"
    assertEquals(
      person.parse[SimpleDimension],
      Left(ParseError.InvalidValue("abc", "long", "id"))
    )
  }
}
