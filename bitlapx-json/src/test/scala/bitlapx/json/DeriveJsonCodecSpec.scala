package bitlapx.json

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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

    println(json1.prettyPrint)
    json1.prettyPrint shouldEqual "{\"Test1\": {\"l\": [1, 2, 3], \"b\": true, \"s\": s, \"d\": 1.0}}"
    back1 shouldEqual Left("Not support type: Test0$1")

    val obj2  = Test2(1, "s", true, List(1, 2, 3))
    val json2 = JsonCodec[Test0].toJson(obj2)
    val back2 = JsonCodec[Test0].fromJson(json2)

    println(json2.prettyPrint)
    json2.prettyPrint shouldEqual "{\"Test2\": {\"l\": [1, 2, 3], \"b\": true, \"s\": s, \"d\": 1.0}}"
    back2 shouldEqual Left("Not support type: Test0$1")
  }
}
