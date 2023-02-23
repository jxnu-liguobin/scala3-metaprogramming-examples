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

import bitlapx.json.adts.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.ListMap

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/21
 */
class JsonCodecSpec extends AnyFlatSpec with Matchers {

  "JsonCodec" should "ok" in {
    final case class Test1(d: Double, s: String, b: Boolean, l: List[Int]) derives JsonCodec

    val obj  = Test1(1, "s", true, List(1, 2, 3))
    val json = JsonCodec[Test1].toJson(obj)
    val back = JsonCodec[Test1].fromJson(json)

    json.prettyPrint shouldEqual "{\"l\": [1, 2, 3], \"b\": true, \"s\": s, \"d\": 1.0}"
    back shouldEqual Right(Test1(1.0, "s", true, List(1, 2, 3)))
    JsonCodec[String].fromJson(Json.Num(2.0)) shouldEqual Left("Expected: String, got: Num(2.0)")
    JsonCodec[Test1].fromJson(Json.Obj(ListMap.empty)) shouldEqual Left("No such element: d")
  }
}
