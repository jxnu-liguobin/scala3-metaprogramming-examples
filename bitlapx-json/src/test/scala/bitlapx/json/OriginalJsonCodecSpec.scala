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
import bitlapx.json.original.*
import bitlapx.json.{ JsonDecoder as _, JsonEncoder as _ }

import scala.collection.immutable.*
import scala.collection.{ immutable, mutable }

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/21
 */
class OriginalJsonCodecSpec extends AnyFlatSpec with Matchers {

  "JsonCodec derives product" should "ok" in {
    final case class Test1(d: Double, s: String, b: Boolean, l: Set[Int])
    val obj  = Test1(1, "s", true, Set(1, 2, 3))
    val json = JsonEncoder.derived[Test1].encode(obj)
    val back = JsonDecoder.derived[Test1].decode(json)
    json.asJsonString shouldEqual "{\"d\": 1.0, \"s\": \"s\", \"b\": true, \"l\": [1, 2, 3]}"
    back shouldEqual Right(Test1(1.0, "s", true, Set(1, 2, 3)))
  }

  "JsonCodec auto derive sum type" should "ok" in {
    sealed trait Test0
    final case class Test1(d: Double, s: String, b: Boolean, l: List[Int]) extends Test0
    val encoder = JsonEncoder.derived[Test0]

    val decoder = JsonDecoder.derived[Test0]

    val obj1  = Test1(1, "s", true, List(1, 2, 3))
    val json1 = encoder.encode(obj1)
    val back1 = decoder.decode(json1)

    println(json1.asJsonString)
    json1.asJsonString shouldEqual "{\"Test1\": {\"d\": 1.0, \"s\": \"s\", \"b\": true, \"l\": [1, 2, 3]}}"
    back1 shouldEqual Right(Test1(1.0, "s", true, List(1, 2, 3)))
  }

  "JsonCodec derives product with jsonExclude" should "ok" in {
    final case class Test1(d: Double, s: String, @jsonExclude b: Boolean)
    val obj1 = Test1(1, "s", true)
    val json = JsonEncoder.derived[Test1].encode(obj1)

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
    final case class Test1(d: Double, s: String, @jsonField("testb") b: Boolean)
    val obj1 = Test1(1, "s", true)
    val json = JsonEncoder.derived[Test1].encode(obj1)

    println(json.asJsonString)
    json.asJsonString shouldEqual "{\"d\": 1.0, \"s\": \"s\", \"testb\": true}"

    val back = JsonDecoder.derived[Test1].decode(json)
    back shouldEqual Right(Test1(1, "s", true))
    json shouldEqual Json.Obj(
      ListMap(
        "d"     -> Json.Num(1.0),
        "s"     -> Json.Str("s"),
        "testb" -> Json.Bool(true)
      )
    )
  }

}
