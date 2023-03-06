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

import bitlapx.json.annotation.{ jsonExclude, jsonField }
import bitlapx.json.ast.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.{ immutable, mutable }
import scala.collection.immutable.*

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/21
 */
class JsonCodecSpec extends AnyFlatSpec with Matchers {

  "JsonCodec derives product" should "ok" in {
    final case class Test1(d: Double, s: String, b: Boolean, l: Set[Int]) derives JsonCodec

    val obj  = Test1(1, "s", true, Set(1, 2, 3))
    val json = JsonCodec[Test1].toJson(obj)
    val back = JsonCodec[Test1].fromJson(json)
    json.asJsonString shouldEqual "{\"d\": 1.0, \"s\": \"s\", \"b\": true, \"l\": [1, 2, 3]}"
    back shouldEqual Right(Test1(1.0, "s", true, Set(1, 2, 3)))
  }

  "JsonCodec derives product with jsonExclude" should "ok" in {
    final case class Test1(d: Double, s: String, @jsonExclude b: Boolean) derives JsonCodec

    val obj1 = Test1(1, "s", true)
    val json = JsonEncoder[Test1].encode(obj1)

    println(json.asJsonString)
    json.asJsonString shouldEqual "{\"d\": 1.0, \"s\": \"s\"}"
    json shouldEqual Json.Obj(
      ListMap(
        "d" -> Json.Num(1.0),
        "s" -> Json.Str("s")
      )
    )
  }

  "JsonCodec derives product with jsonField" should "ok" in {
    final case class Test1(d: Double, s: String, @jsonField("testb") b: Boolean) derives JsonCodec
    val obj1 = Test1(1, "s", true)
    val json = JsonEncoder[Test1].encode(obj1)

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

//  "JsonCodec derives sum type" should "ok" in {
//    sealed trait Test0 derives  JsonEncoder
//    final case class Test1(d: Double, s: String, b: Boolean, l: List[Int]) extends Test0
//    final case class Test2(d: Double, s: String, b: Boolean, l: List[Int]) extends Test0
//
//
//    val obj1 = Test1(1, "s", true, List(1, 2, 3))
//    val json1 = JsonEncoder[Test0].encode(obj1)
//    val back1 = JsonDecoder[Test0].decode(json1)
//
//
//    println(json1.asJsonString)
//    json1.asJsonString shouldEqual "{\"Test1\": {\"d\": 1.0, \"s\": \"s\", \"b\": true, \"l\": [1, 2, 3]}}"
//    back1 shouldEqual Right(Test1(1.0, "s", true, List(1, 2, 3)))
//
//    val obj2 = Test2(1, "s", true, List(1, 2, 3))
//    val json2 = JsonCodec[Test0].toJson(obj2)
//    val back2 = JsonCodec[Test0].fromJson(json2)
//
//    json2.asJsonString shouldEqual "{\"Test2\": {\"d\": 1.0, \"s\": \"s\", \"b\": true, \"l\": [1, 2, 3]}}"
//    back2 shouldEqual Right(Test2(1.0, "s", true, List(1, 2, 3)))
//  }

  "JsonCodec string from num" should "fail" in {
    JsonCodec[String].fromJson(Json.Num(2.0)) shouldEqual Left("Expected: String, got: Num(2.0)")
  }

  "JsonCodec string" should "ok" in {
    JsonCodec[String].fromJson(Json.Str("hello world")) shouldEqual Right("hello world")
  }
}
