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

package bitlapx.common

import SimpleUtils.*
import magnolia1.TypeInfo
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.quoted.*

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/21
 */
class SyntaxSpec extends AnyFlatSpec with Matchers {

  "show" should "ok" in {
    given Encoder[Test1, String] = Encoder.derived[Test1]
    given Encoder[Test2, String] = Encoder.derived[Test2]
    given Encoder[Test3, String] = Encoder.derived[Test3]

    val fs = labels[Test2]
    val s1 = summon[Encoder[Test1, String]].encode(Test1("x", 20))
    val s2 = summon[Encoder[Test2, String]].encode(Test2("xyz", 100L))
    val s3 = summon[Encoder[Test3, String]].encode(Test3(5, Test2("abc", 200L)))
    fs shouldEqual List("token", "tx")
    s1 shouldEqual "Test1(f1=x,f2=20)"
    s2 shouldEqual "Test2(token=***,tx=100L)"
    s3 shouldEqual "Test3(x=5,t2=Test2(token=***,tx=200L))"
  }

  "TypeNameInfo" should "ok" in {
    val s1 = summon[TypeNameInfo[Test1]]
    s1.name shouldEqual TypeInfo("bitlapx.common", "Test1", List())
    s1.subtypeNames shouldEqual List()
  }

  "debug" should "compile pass" in {
    """
      |def hello =
      |   CaseClassA(1, "hello", CaseClassB(1))
      |
      |debug(hello)""".stripMargin should compile
  }
}
