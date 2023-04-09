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

import bitlapx.json.annotation.*
import bitlapx.json.ast.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.*
import scala.collection.{ immutable, mutable }

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/21
 */
class OriginalJsonCodecSpec extends AnyFlatSpec with Matchers {

  "JsonCodec sum type with nested class" should "ok" in {
    sealed trait Test0
    final case class Test1(d: Double, s: String, b: Boolean, l: List[Test2]) extends Test0
    final case class Test2(d: Double, s: String, b: Option[Boolean])

    given JsonCodec[Test0] = JsonCodec(
      JsonEncoder.derived[Test0],
      JsonDecoder.derived[Test0]
    )

    given JsonEncoder[Test1] = original.JsonEncoder.derived[Test1]
    given JsonDecoder[Test1] = original.JsonDecoder.derived[Test1]
    given JsonEncoder[Test2] = original.JsonEncoder.derived[Test2]
    given JsonDecoder[Test2] = original.JsonDecoder.derived[Test2]

    val obj1  = Test1(1, "s", true, List(Test2(0.1, "212", None)))
    val json1 = JsonCodec[Test0].toJson(obj1)
    val back1 = JsonCodec[Test0].fromJson(json1)

    println(json1.asJsonString)
    json1.asJsonString shouldEqual "{\"Test1\": {\"d\": 1.0, \"s\": \"s\", \"b\": true, \"l\": [{\"d\": 0.1, \"s\": \"212\", \"b\": null}]}}"
    back1 shouldEqual Right(obj1)
  }

  "JsonCodec original product" should "ok" in {
    final case class Test1(d: Double, s: String, b: Boolean, l: Set[Int])
    val obj = Test1(1, "s", true, Set(1, 2, 3))
    given JsonCodec[Test1] = JsonCodec(
      JsonEncoder.derived[Test1],
      JsonDecoder.derived[Test1]
    )

    val json = JsonCodec[Test1].toJson(obj)
    val back = JsonCodec[Test1].fromJson(json)
    json.asJsonString shouldEqual "{\"d\": 1.0, \"s\": \"s\", \"b\": true, \"l\": [1, 2, 3]}"
    back shouldEqual Right(Test1(1.0, "s", true, Set(1, 2, 3)))
  }

  "JsonCodec original sum type" should "ok" in {
    sealed trait Test0
    final case class Test1(d: Double, s: String, b: Boolean, l: List[Int]) extends Test0
    given JsonCodec[Test0] = JsonCodec(
      JsonEncoder.derived[Test0],
      JsonDecoder.derived[Test0]
    )
    given JsonEncoder[Test1] = original.JsonEncoder.derived[Test1]
    given JsonDecoder[Test1] = original.JsonDecoder.derived[Test1]

    val obj1  = Test1(1, "s", true, List(1, 2, 3))
    val json1 = JsonCodec[Test0].toJson(obj1)
    val back1 = JsonCodec[Test0].fromJson(json1)

    println(json1.asJsonString)
    json1.asJsonString shouldEqual "{\"Test1\": {\"d\": 1.0, \"s\": \"s\", \"b\": true, \"l\": [1, 2, 3]}}"
    back1 shouldEqual Right(Test1(1.0, "s", true, List(1, 2, 3)))
  }

  "JsonCodec original product with jsonExclude" should "ok" in {
    final case class Test1(d: Double, s: String, @jsonExclude b: Boolean)
    val obj1 = Test1(1, "s", true)
    val json = original.JsonEncoder.derived[Test1].encode(obj1)

    println(json.asJsonString)
    json.asJsonString shouldEqual "{\"d\": 1.0, \"s\": \"s\"}"
    json shouldEqual Json.Obj(
      ListMap(
        "d" -> Json.Num(1.0),
        "s" -> Json.Str("s")
      )
    )
  }

  "JsonCodec original product with jsonField" should "ok" in {
    final case class Test1(d: Double, s: String, @jsonField("testb") b: Boolean)
    val obj1 = Test1(1, "s", true)

    given JsonCodec[Test1] = JsonCodec(
      JsonEncoder.derived[Test1],
      JsonDecoder.derived[Test1]
    )

    val json = JsonCodec[Test1].toJson(obj1)
    println(json.asJsonString)
    json.asJsonString shouldEqual "{\"d\": 1.0, \"s\": \"s\", \"testb\": true}"

    val back = JsonCodec[Test1].fromJson(json)
    back shouldEqual Right(Test1(1, "s", true))
    json shouldEqual Json.Obj(
      ListMap(
        "d"     -> Json.Num(1.0),
        "s"     -> Json.Str("s"),
        "testb" -> Json.Bool(true)
      )
    )
  }

  "JsonCodec original sum type" should "error" in {
    sealed trait Test0
    final case class Test1(d: Double, s: String, b: Boolean) extends Test0

    given JsonCodec[Test0] = JsonCodec(
      original.JsonEncoder.derived[Test0],
      original.JsonDecoder.derived[Test0]
    )
    given JsonEncoder[Test1] = original.JsonEncoder.derived[Test1]
    given JsonDecoder[Test1] = original.JsonDecoder.derived[Test1]

    val json = Json.Obj(
      ListMap(
        "d" -> Json.Num(1.0),
        "s" -> Json.Str("s"),
        "b" -> Json.Bool(true)
      )
    )
    val back1 = JsonCodec[Test0].fromJson(json)

    back1 shouldEqual Left("""Invalid json obj: {"d": 1.0, "s": "s", "b": true}, cannot be a sum type""")

    val json2 = Json.Obj(
      ListMap(
        "Test0" -> Json.Obj(ListMap("d" -> Json.Num(1.0), "s" -> Json.Str("s"), "b" -> Json.Bool(true)))
      )
    )

    val back2 = JsonCodec[Test0].fromJson(json2)
    back2 shouldEqual Left(
      """Invalid json obj: {"Test0": {"d": 1.0, "s": "s", "b": true}}, cannot be a sum type, available subTypes: Test1"""
    )

  }
}
