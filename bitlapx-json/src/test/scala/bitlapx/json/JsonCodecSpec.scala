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

  "JsonCodec derives nested product by Set" should "ok" in {
    final case class Test1(d: Double, s: String, b: Boolean, l: Set[Test2]) derives JsonCodec
    final case class Test2(abc: String)

    val obj1 = Test1(1, "s", true, Set(Test2("abc")))
    val json = JsonCodec[Test1].toJson(obj1)
    val back = JsonCodec[Test1].fromJson(json)

    println(json.asJsonString)

    json.asJsonString shouldEqual "{\"d\": 1.0, \"s\": \"s\", \"b\": true, \"l\": [{\"abc\": \"abc\"}]}"
    back shouldEqual Right(Test1(1, "s", true, Set(Test2("abc"))))
  }

  "JsonCodec derives nested products by all collections" should "ok" in {
    final case class Test2(abc: String)
    object Test2 {
      given Ordering[Test2] = scala.Ordering.fromLessThan((a, b) => a.abc.last < b.abc.last)
    }
    final case class Test1(
      d: Double,
      s: String,
      b: Boolean,
      hashKey: HashMap[String, Test2],
      vectorKey: Vector[Test2],
      eitherKey: Either[String, Test2],
      hashSetKey: HashSet[Test2],
      mapKey: Map[String, Test2],
      treeSetKey: SortedSet[Test2], // TreeSet
      linearSeqKey: LinearSeq[Test2],
      indexedSeqKey: IndexedSeq[Test2],
      sortedSetKey: SortedSet[Test2],
      listSetKey: ListSet[Test2],
      mutableMapKey: mutable.Map[String, Test2]
    ) derives JsonCodec

    val obj1 = Test1(
      1,
      "s",
      true,
      HashMap("1" -> Test2("abc"), "2" -> Test2("abc")),
      Vector(Test2("vector value")),
      Right(Test2("either value")).withLeft[String],
      HashSet(Test2("hashset value 1"), Test2("hashset value 2")),
      Map("1" -> Test2("map value 1"), "2" -> Test2("map value 2")),
      TreeSet(Test2("tree map value1"), Test2("tree map value2")),
      LinearSeq(Test2("liner seq value1"), Test2("liner seq value2")),
      IndexedSeq(Test2("indexed seq value1"), Test2("indexed seq value2")),
      SortedSet(Test2("sorted set value1"), Test2("sorted set value2")),
      ListSet(Test2("list set value1"), Test2("list set value2")),
      mutable.Map("1" -> Test2("mutable map value1"), "2" -> Test2("mutable map value2"))
    )

    val json = JsonCodec[Test1].toJson(obj1)
    val back = JsonCodec[Test1].fromJson(json)

    Right(obj1) shouldEqual back
  }

  "JsonCodec string from num" should "fail" in {
    JsonCodec[String].fromJson(Json.Num(2.0)) shouldEqual Left("Expected: String, got: Num(2.0)")
  }

  "JsonCodec string" should "ok" in {
    JsonCodec[String].fromJson(Json.Str("hello world")) shouldEqual Right("hello world")
  }
}
