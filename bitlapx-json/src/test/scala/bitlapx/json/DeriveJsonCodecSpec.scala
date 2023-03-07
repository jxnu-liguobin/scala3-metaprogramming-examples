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
import bitlapx.json.ast.Json
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.{ immutable, mutable }
import scala.collection.immutable.*
import scala.math

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/23
 */
class DeriveJsonCodecSpec extends AnyFlatSpec with Matchers {

  "DeriveJsonCodec sum type" should "ok" in {
    sealed trait Test0
    final case class Test1(d: Double, s: String, b: Boolean, l: List[Int]) extends Test0
    final case class Test2(d: Double, s: String, b: Boolean, l: List[Int]) extends Test0

    given JsonCodec[Test0] = DeriveJsonCodec.gen[Test0]

    val obj1  = Test1(1, "s", true, List(1, 2, 3))
    val json1 = JsonCodec[Test0].toJson(obj1)
    val back1 = JsonCodec[Test0].fromJson(json1)

    println(json1.asJsonString)
    json1.asJsonString shouldEqual "{\"Test1\": {\"d\": 1.0, \"s\": \"s\", \"b\": true, \"l\": [1, 2, 3]}}"
    back1 shouldEqual Right(Test1(1.0, "s", true, List(1, 2, 3)))

    val obj2  = Test2(1, "s", true, List(1, 2, 3))
    val json2 = JsonCodec[Test0].toJson(obj2)
    val back2 = JsonCodec[Test0].fromJson(json2)

    json2.asJsonString shouldEqual "{\"Test2\": {\"d\": 1.0, \"s\": \"s\", \"b\": true, \"l\": [1, 2, 3]}}"
    back2 shouldEqual Right(Test2(1.0, "s", true, List(1, 2, 3)))
  }

  "JsonCodec nested product by List" should "ok" in {
    final case class Test1(d: Double, s: String, b: Boolean, l: List[Test2])
    final case class Test2(abc: String)
    given JsonCodec[Test1] = DeriveJsonCodec.gen[Test1]

    val obj1 = Test1(1, "s", true, List(Test2("abc")))
    val json = JsonCodec[Test1].toJson(obj1)
    val back = JsonCodec[Test1].fromJson(json)

    println(json.asJsonString)
    json.asJsonString shouldEqual "{\"d\": 1.0, \"s\": \"s\", \"b\": true, \"l\": [{\"abc\": \"abc\"}]}"
    back shouldEqual Right(Test1(1, "s", true, List(Test2("abc"))))
  }

  "JsonCodec nested product by Array" should "ok" in {
    final case class Test1(d: Double, s: String, b: Boolean, l: Array[Test2])
    final case class Test2(abc: String)

    given JsonCodec[Test1] = DeriveJsonCodec.gen[Test1]

    val obj1 = Test1(1.0, "s", true, Array(Test2("abc")))
    val json = JsonCodec[Test1].toJson(obj1)
    val back = JsonCodec[Test1].fromJson(json)

    println(json.asJsonString)

    json.asJsonString shouldEqual "{\"d\": 1.0, \"s\": \"s\", \"b\": true, \"l\": [{\"abc\": \"abc\"}]}"

    // array not equals
    back.map(_.s) shouldEqual Right(Test1(1.0, "s", true, Array(Test2("abc")))).map(_.s)
  }

  "JsonCodec nested product by Seq" should "ok" in {
    final case class Test1(d: Double, s: String, b: Boolean, l: Seq[Test2])
    final case class Test2(abc: String)

    given JsonCodec[Test1] = DeriveJsonCodec.gen[Test1]

    val obj1 = Test1(1, "s", true, Seq(Test2("abc")))
    val json = JsonCodec[Test1].toJson(obj1)
    val back = JsonCodec[Test1].fromJson(json)

    println(json.asJsonString)
    json.asJsonString shouldEqual "{\"d\": 1.0, \"s\": \"s\", \"b\": true, \"l\": [{\"abc\": \"abc\"}]}"
    back shouldEqual Right(Test1(1, "s", true, Seq(Test2("abc"))))
  }

  "JsonCodec nested product by IndexedSeq" should "ok" in {
    final case class Test1(d: Double, s: String, b: Boolean, l: LinearSeq[Test2])
    final case class Test2(abc: String)

    given JsonCodec[Test1] = DeriveJsonCodec.gen[Test1]

    val obj1 = Test1(1, "s", true, LinearSeq(Test2("abc")))
    val json = JsonCodec[Test1].toJson(obj1)
    val back = JsonCodec[Test1].fromJson(json)

    println(json.asJsonString)
    json.asJsonString shouldEqual "{\"d\": 1.0, \"s\": \"s\", \"b\": true, \"l\": [{\"abc\": \"abc\"}]}"
    back shouldEqual Right(Test1(1, "s", true, LinearSeq(Test2("abc"))))
  }

  "JsonCodec nested product by HashMap" should "ok" in {
    final case class Test1(d: Double, s: String, b: Boolean, l: HashMap[String, Test2])
    final case class Test2(abc: String)

    given JsonCodec[Test1] = DeriveJsonCodec.gen[Test1]

    val obj1 = Test1(1, "s", true, HashMap("1" -> Test2("abc")))
    val json = JsonCodec[Test1].toJson(obj1)
    val back = JsonCodec[Test1].fromJson(json)

    println(json.asJsonString)
    json.asJsonString shouldEqual "{\"d\": 1.0, \"s\": \"s\", \"b\": true, \"l\": {\"1\": {\"abc\": \"abc\"}}}"
    back shouldEqual Right(Test1(1, "s", true, HashMap("1" -> Test2("abc"))))
  }

  "JsonCodec nested product by Set" should "ok" in {
    final case class Test1(d: Double, s: String, b: Boolean, l: Set[Test2])
    final case class Test2(abc: String)

    given JsonCodec[Test1] = DeriveJsonCodec.gen[Test1]

    val obj1 = Test1(1, "s", true, Set(Test2("abc")))
    val json = JsonCodec[Test1].toJson(obj1)
    val back = JsonCodec[Test1].fromJson(json)

    println(json.asJsonString)
    json.asJsonString shouldEqual "{\"d\": 1.0, \"s\": \"s\", \"b\": true, \"l\": [{\"abc\": \"abc\"}]}"
    back shouldEqual Right(Test1(1, "s", true, Set(Test2("abc"))))
  }

  "JsonCodec nested product by IndexedSeq with two elements" should "ok" in {
    final case class Test1(d: Double, s: String, b: Boolean, l: LinearSeq[Test2])
    final case class Test2(abc: String)

    given JsonCodec[Test1] = DeriveJsonCodec.gen[Test1]

    val obj1 = Test1(1, "s", true, LinearSeq(Test2("abc"), Test2("dgf")))
    val json = JsonCodec[Test1].toJson(obj1)
    val back = JsonCodec[Test1].fromJson(json)

    println(json.asJsonString)
    json.asJsonString shouldEqual "{\"d\": 1.0, \"s\": \"s\", \"b\": true, \"l\": [{\"abc\": \"abc\"}, {\"abc\": \"dgf\"}]}"
    back shouldEqual Right(Test1(1.0, "s", true, List(Test2("abc"), Test2("dgf"))))
  }

  "JsonCodec nested product by HashMap with two elements" should "ok" in {
    final case class Test1(d: Double, s: String, b: Boolean, l: HashMap[String, Test2])
    final case class Test2(abc: String)

    given JsonCodec[Test1] = DeriveJsonCodec.gen[Test1]

    val obj1 = Test1(1, "s", true, HashMap("1" -> Test2("abc"), "2" -> Test2("abc")))
    val json = JsonCodec[Test1].toJson(obj1)
    val back = JsonCodec[Test1].fromJson(json)

    println(json.asJsonString)
    json.asJsonString shouldEqual "{\"d\": 1.0, \"s\": \"s\", \"b\": true, \"l\": {\"1\": {\"abc\": \"abc\"}, \"2\": {\"abc\": \"abc\"}}}"
    back shouldEqual Right(Test1(1, "s", true, HashMap("1" -> Test2("abc"), "2" -> Test2("abc"))))
  }

  "JsonCodec nested products by all collections" should "ok" in {
    final case class Test1(
      d: Double,
      s: String,
      b: Boolean,
      hashKey: HashMap[String, Test2],
      vectorKey: Vector[Test2],
      eitherKey: Either[String, Test2],
      hashSetKey: HashSet[Test2],
      mapKey: Map[String, Test2],
      treeSetKey: TreeSet[Test2],
      linearSeqKey: LinearSeq[Test2],
      indexedSeqKey: IndexedSeq[Test2],
      sortedSetKey: SortedSet[Test2],
      listSetKey: ListSet[Test2],
      mutableMapKey: mutable.Map[String, Test2]
    )
    final case class Test2(abc: String)
    object Test2 {
      given Ordering[Test2] = scala.Ordering.fromLessThan((a, b) => a.abc.last < b.abc.last)
    }

    given JsonCodec[Test1] = DeriveJsonCodec.gen[Test1]

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

    println(json.asJsonString)
    json.asJsonString shouldEqual "{\"d\": 1.0, \"s\": \"s\", \"b\": true, \"hashKey\": {\"1\": {\"abc\": \"abc\"}, \"2\": {\"abc\": \"abc\"}}, \"vectorKey\": [{\"abc\": \"vector value\"}], \"eitherKey\": {\"Right\": {\"abc\": \"either value\"}}, \"hashSetKey\": [{\"abc\": \"hashset value 1\"}, {\"abc\": \"hashset value 2\"}], \"mapKey\": {\"1\": {\"abc\": \"map value 1\"}, \"2\": {\"abc\": \"map value 2\"}}, \"treeSetKey\": [{\"abc\": \"tree map value1\"}, {\"abc\": \"tree map value2\"}], \"linearSeqKey\": [{\"abc\": \"liner seq value1\"}, {\"abc\": \"liner seq value2\"}], \"indexedSeqKey\": [{\"abc\": \"indexed seq value1\"}, {\"abc\": \"indexed seq value2\"}], \"sortedSetKey\": [{\"abc\": \"sorted set value1\"}, {\"abc\": \"sorted set value2\"}], \"listSetKey\": [{\"abc\": \"list set value1\"}, {\"abc\": \"list set value2\"}], \"mutableMapKey\": {\"1\": {\"abc\": \"mutable map value1\"}, \"2\": {\"abc\": \"mutable map value2\"}}}"

    println(JsonPrettyPrinter.prettyPrintJson(json.asJsonString))
    JsonPrettyPrinter.prettyPrintJson(json.asJsonString) shouldEqual
      """{
        |    "d":  1.0,
        |    "s":  "s",
        |    "b":  true,
        |    "hashKey":  {
        |        "1":  {
        |            "abc":  "abc"
        |        },
        |        "2":  {
        |            "abc":  "abc"
        |        }
        |    },
        |    "vectorKey":  [
        |        {
        |            "abc":  "vector value"
        |        }
        |    ],
        |    "eitherKey":  {
        |        "Right":  {
        |            "abc":  "either value"
        |        }
        |    },
        |    "hashSetKey":  [
        |        {
        |            "abc":  "hashset value 1"
        |        },
        |        {
        |            "abc":  "hashset value 2"
        |        }
        |    ],
        |    "mapKey":  {
        |        "1":  {
        |            "abc":  "map value 1"
        |        },
        |        "2":  {
        |            "abc":  "map value 2"
        |        }
        |    },
        |    "treeSetKey":  [
        |        {
        |            "abc":  "tree map value1"
        |        },
        |        {
        |            "abc":  "tree map value2"
        |        }
        |    ],
        |    "linearSeqKey":  [
        |        {
        |            "abc":  "liner seq value1"
        |        },
        |        {
        |            "abc":  "liner seq value2"
        |        }
        |    ],
        |    "indexedSeqKey":  [
        |        {
        |            "abc":  "indexed seq value1"
        |        },
        |        {
        |            "abc":  "indexed seq value2"
        |        }
        |    ],
        |    "sortedSetKey":  [
        |        {
        |            "abc":  "sorted set value1"
        |        },
        |        {
        |            "abc":  "sorted set value2"
        |        }
        |    ],
        |    "listSetKey":  [
        |        {
        |            "abc":  "list set value1"
        |        },
        |        {
        |            "abc":  "list set value2"
        |        }
        |    ],
        |    "mutableMapKey":  {
        |        "1":  {
        |            "abc":  "mutable map value1"
        |        },
        |        "2":  {
        |            "abc":  "mutable map value2"
        |        }
        |    }
        |}""".stripMargin
    back shouldEqual Right(obj1)
  }

  "JsonCodec jsonExclude" should "ok" in {
    final case class Test1(d: Double, s: String, b: Boolean, @jsonExclude l: List[Test2])
    final case class Test2(abc: String)

    given JsonCodec[Test1] = DeriveJsonCodec.gen[Test1]

    val obj1 = Test1(1, "s", true, List(Test2("abc")))
    val json = JsonCodec[Test1].toJson(obj1)

    println(json.asJsonString)
    json.asJsonString shouldEqual "{\"d\": 1.0, \"s\": \"s\", \"b\": true}"
    json shouldEqual Json.Obj(
      ListMap(
        "d" -> Json.Num(1.0),
        "s" -> Json.Str("s"),
        "b" -> Json.Bool(true)
      )
    )
    Json
      .Obj(
        ListMap(
          "d" -> Json.Num(1.0),
          "s" -> Json.Str("s"),
          "b" -> Json.Bool(true)
        )
      )
      .selectDynamic("d") shouldEqual Json.Num(1.0)
  }

  "JsonCodec jsonField" should "ok" in {
    final case class Test1(d: Double, s: String, b: Boolean, @jsonField("test2-list") l: List[Test2])
    final case class Test2(abc: String)

    given JsonCodec[Test1] = DeriveJsonCodec.gen[Test1]

    val obj1 = Test1(1, "s", true, List(Test2("abc")))
    val json = JsonCodec[Test1].toJson(obj1)

    println(json.asJsonString)
    json.asJsonString shouldEqual "{\"d\": 1.0, \"s\": \"s\", \"b\": true, \"test2-list\": [{\"abc\": \"abc\"}]}"

    val back = JsonCodec[Test1].fromJson(json)

    back shouldEqual Right(Test1(1, "s", true, List(Test2("abc"))))
    json shouldEqual Json.Obj(
      ListMap(
        "d"          -> Json.Num(1.0),
        "s"          -> Json.Str("s"),
        "b"          -> Json.Bool(true),
        "test2-list" -> Json.Arr(List(Json.Obj(ListMap("abc" -> Json.Str("abc")))))
      )
    )
  }
}
