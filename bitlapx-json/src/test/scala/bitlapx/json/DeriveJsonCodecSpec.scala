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

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.immutable
import scala.collection.immutable.*

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

    json1.prettyPrint shouldEqual "{\"Test1\": {\"l\": [1, 2, 3], \"b\": true, \"s\": s, \"d\": 1.0}}"
    back1 shouldEqual Right(Test1(1.0, "s", true, List(1, 2, 3)))

    val obj2  = Test2(1, "s", true, List(1, 2, 3))
    val json2 = JsonCodec[Test0].toJson(obj2)
    val back2 = JsonCodec[Test0].fromJson(json2)

    json2.prettyPrint shouldEqual "{\"Test2\": {\"l\": [1, 2, 3], \"b\": true, \"s\": s, \"d\": 1.0}}"
    back2 shouldEqual Right(Test2(1.0, "s", true, List(1, 2, 3)))
  }

  "JsonCodec nested product by List" should "ok" in {
    final case class Test1(d: Double, s: String, b: Boolean, l: List[Test2])
    final case class Test2(abc: String)
    given JsonCodec[Test1] = DeriveJsonCodec.gen[Test1]

    val obj1 = Test1(1, "s", true, List(Test2("abc")))
    val json = JsonCodec[Test1].toJson(obj1)
    val back = JsonCodec[Test1].fromJson(json)

    println(json.prettyPrint)
    json.prettyPrint shouldEqual "{\"d\": 1.0, \"s\": s, \"b\": true, \"l\": [{\"abc\": abc}]}"
    back shouldEqual Right(Test1(1, "s", true, List(Test2("abc"))))
  }

  "JsonCodec nested product by Array" should "ok" in {
    final case class Test1(d: Double, s: String, b: Boolean, l: Array[Test2])
    final case class Test2(abc: String)

    given JsonCodec[Test1] = DeriveJsonCodec.gen[Test1]

    val obj1 = Test1(1.0, "s", true, Array(Test2("abc")))
    val json = JsonCodec[Test1].toJson(obj1)
    val back = JsonCodec[Test1].fromJson(json)

    println(json.prettyPrint)

    json.prettyPrint shouldEqual "{\"d\": 1.0, \"s\": s, \"b\": true, \"l\": [{\"abc\": abc}]}"

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

    println(json.prettyPrint)
    json.prettyPrint shouldEqual "{\"d\": 1.0, \"s\": s, \"b\": true, \"l\": [{\"abc\": abc}]}"
    back shouldEqual Right(Test1(1, "s", true, Seq(Test2("abc"))))
  }

  "JsonCodec nested product by IndexedSeq" should "ok" in {
    final case class Test1(d: Double, s: String, b: Boolean, l: LinearSeq[Test2])
    final case class Test2(abc: String)

    given JsonCodec[Test1] = DeriveJsonCodec.gen[Test1]

    val obj1 = Test1(1, "s", true, LinearSeq(Test2("abc")))
    val json = JsonCodec[Test1].toJson(obj1)
    val back = JsonCodec[Test1].fromJson(json)

    println(json.prettyPrint)
    json.prettyPrint shouldEqual "{\"d\": 1.0, \"s\": s, \"b\": true, \"l\": [{\"abc\": abc}]}"
    back shouldEqual Right(Test1(1, "s", true, LinearSeq(Test2("abc"))))
  }

  "JsonCodec nested product by HashMap" should "ok" in {
    final case class Test1(d: Double, s: String, b: Boolean, l: HashMap[String, Test2])
    final case class Test2(abc: String)

    given JsonCodec[Test1] = DeriveJsonCodec.gen[Test1]

    val obj1 = Test1(1, "s", true, HashMap("1" -> Test2("abc")))
    val json = JsonCodec[Test1].toJson(obj1)
    val back = JsonCodec[Test1].fromJson(json)

    println(json.prettyPrint)
    json.prettyPrint shouldEqual "{\"d\": 1.0, \"s\": s, \"b\": true, \"l\": {\"1\": {\"abc\": abc}}}"
    back shouldEqual Right(Test1(1, "s", true, HashMap("1" -> Test2("abc"))))
  }

  "JsonCodec nested product by Set" should "ok" in {
    final case class Test1(d: Double, s: String, b: Boolean, l: Set[Test2])
    final case class Test2(abc: String)

    given JsonCodec[Test1] = DeriveJsonCodec.gen[Test1]

    val obj1 = Test1(1, "s", true, Set(Test2("abc")))
    val json = JsonCodec[Test1].toJson(obj1)
    val back = JsonCodec[Test1].fromJson(json)

    println(json.prettyPrint)
    json.prettyPrint shouldEqual "{\"d\": 1.0, \"s\": s, \"b\": true, \"l\": [{\"abc\": abc}]}"
    back shouldEqual Right(Test1(1, "s", true, Set(Test2("abc"))))
  }


  "JsonCodec nested product by IndexedSeq with two elements" should "ok" in {
    final case class Test1(d: Double, s: String, b: Boolean, l: LinearSeq[Test2])
    final case class Test2(abc: String)

    given JsonCodec[Test1] = DeriveJsonCodec.gen[Test1]

    val obj1 = Test1(1, "s", true, LinearSeq(Test2("abc"),Test2("dgf")))
    val json = JsonCodec[Test1].toJson(obj1)
    val back = JsonCodec[Test1].fromJson(json)

    println(json.prettyPrint)
    json.prettyPrint shouldEqual "{\"d\": 1.0, \"s\": s, \"b\": true, \"l\": [{\"abc\": abc}, {\"abc\": dgf}]}"
    back shouldEqual Right(Test1(1.0, "s", true, List(Test2("abc"), Test2("dgf"))))
  }

  "JsonCodec nested product by HashMap with two elements" should "ok" in {
    final case class Test1(d: Double, s: String, b: Boolean, l: HashMap[String, Test2])
    final case class Test2(abc: String)

    given JsonCodec[Test1] = DeriveJsonCodec.gen[Test1]

    val obj1 = Test1(1, "s", true, HashMap("1" -> Test2("abc"),"2" -> Test2("abc")))
    val json = JsonCodec[Test1].toJson(obj1)
    val back = JsonCodec[Test1].fromJson(json)

    println(json.prettyPrint)
    json.prettyPrint shouldEqual "{\"d\": 1.0, \"s\": s, \"b\": true, \"l\": {\"1\": {\"abc\": abc}, \"2\": {\"abc\": abc}}}"
    back shouldEqual Right(Test1(1, "s", true, HashMap("1" -> Test2("abc"),"2" -> Test2("abc"))))
  }

}
