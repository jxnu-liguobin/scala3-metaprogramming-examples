package bitlapx.json

import bitlapx.common.Bitlapx
import adts.*
import bitlapx.json
import magnolia1.{ AutoDerivation, CaseClass, SealedTrait }

import scala.collection.immutable.ListMap
import scala.compiletime.*
import scala.deriving.*
import scala.deriving.Mirror

trait JsonEncoder[A]:

  def encode(a: A): Json

end JsonEncoder

object JsonEncoder:

  given JavaBigDecimalEncoder: JsonEncoder[java.math.BigDecimal] = (v: java.math.BigDecimal) => Json.Num(v)

  given SymbolEncoder: JsonEncoder[Symbol] = (v: Symbol) => Json.Str(v.name)

  given BoolEncoder: JsonEncoder[Boolean] = (v: Boolean) => Json.Bool(v)

  given StringEncoder: JsonEncoder[String] = (v: String) => Json.Str(v)

  given ShortEncoder: JsonEncoder[Short] = (v: Short) => Json.Num(v)

  given LongEncoder: JsonEncoder[Long] = (v: Long) => Json.Num(v)

  given ByteEncoder: JsonEncoder[Byte] = (v: Byte) => Json.Num(v)

  given IntEncoder: JsonEncoder[Int] = (v: Int) => Json.Num(v)

  given BigDecimalEncoder: JsonEncoder[BigDecimal] = (v: BigDecimal) => Json.Num(v)

  given FloatEncoder: JsonEncoder[Float] = (v: Float) => Json.Num(v)

  given DoubleEncoder: JsonEncoder[Double] = (v: Double) => Json.Num(v)

  given ArrEncoder[V](using js: JsonEncoder[V]): JsonEncoder[List[V]] = (list: List[V]) => Json.Arr(list map js.encode)

  inline given derived[V](using m: Mirror.Of[V]): JsonEncoder[V] = (v: V) =>
    Json.Obj(toListMap[m.MirroredElemTypes, m.MirroredElemLabels, V](v, 0))

  private inline def toListMap[T, L, V](v: V, i: Int): ListMap[String, Json] = inline erasedValue[(T, L)] match
    case _: (EmptyTuple, EmptyTuple) => ListMap.empty
    case _: (t *: ts, l *: ls) =>
      val js    = summonInline[JsonEncoder[t]]
      val label = constValue[l].asInstanceOf[String]
      val value = js.encode(Bitlapx.productElement[t](v, i))

      toListMap[ts, ls, V](v, i + 1) + (label -> value)
