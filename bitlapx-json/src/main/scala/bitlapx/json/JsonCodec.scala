package bitlapx.json

import bitlapx.common.Bitlapx
import bitlapx.json.adts.*

import scala.collection.immutable.ListMap
import scala.compiletime.*
import scala.deriving.*
import scala.reflect.ClassTag
import scala.runtime.Tuples
import scala.util.{ Either, Right }

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/23
 */
final class JsonCodec[V](e: JsonEncoder[V], d: JsonDecoder[V]):
  def toJson(v: V): Json              = e.encode(v)
  def fromJson(json: Json): Result[V] = d.decode(json)
end JsonCodec

object JsonCodec:
  inline def apply[V](e: JsonEncoder[V], d: JsonDecoder[V]): JsonCodec[V] = new JsonCodec(e, d)
  inline def apply[V](using js: JsonCodec[V]): JsonCodec[V]               = js
  inline given derived[V](using m: Mirror.Of[V], e: JsonEncoder[V], d: JsonDecoder[V]): JsonCodec[V] =
    new JsonCodec[V](e, d)

  given string: JsonCodec[String] = new JsonCodec[String](JsonEncoder.StringEncoder, JsonDecoder.StringDecoder)
  given int: JsonCodec[Int]       = new JsonCodec[Int](JsonEncoder.IntEncoder, JsonDecoder.IntDecoder)
  given short: JsonCodec[Short]   = new JsonCodec[Short](JsonEncoder.ShortEncoder, JsonDecoder.ShortDecoder)
  given long: JsonCodec[Long]     = new JsonCodec[Long](JsonEncoder.LongEncoder, JsonDecoder.LongDecoder)
  given float: JsonCodec[Float]   = new JsonCodec[Float](JsonEncoder.FloatEncoder, JsonDecoder.FloatDecoder)
  given double: JsonCodec[Double] = new JsonCodec[Double](JsonEncoder.DoubleEncoder, JsonDecoder.DoubleDecoder)
  given byte: JsonCodec[Byte]     = new JsonCodec[Byte](JsonEncoder.ByteEncoder, JsonDecoder.ByteDecoder)
  given bigDecimal: JsonCodec[BigDecimal] =
    new JsonCodec[BigDecimal](JsonEncoder.BigDecimalEncoder, JsonDecoder.BigDecimalDecoder)

  given javaBigDecimal: JsonCodec[java.math.BigDecimal] =
    new JsonCodec[java.math.BigDecimal](JsonEncoder.JavaBigDecimalEncoder, JsonDecoder.JavaBigDecimalDecoder)

  given boolean: JsonCodec[Boolean] = new JsonCodec[Boolean](JsonEncoder.BoolEncoder, JsonDecoder.BoolDecoder)
  given symbol: JsonCodec[Symbol]   = new JsonCodec[Symbol](JsonEncoder.SymbolEncoder, JsonDecoder.SymbolDecoder)
