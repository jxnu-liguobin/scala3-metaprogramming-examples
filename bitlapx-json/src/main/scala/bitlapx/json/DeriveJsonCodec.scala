package bitlapx.json

import bitlapx.json.adts.*
import magnolia1.{ AutoDerivation, CaseClass, SealedTrait }

import scala.collection.immutable.ListMap
import scala.deriving.Mirror
import scala.reflect.ClassTag

object DeriveJsonCodec:
  inline def gen[A](using mirror: Mirror.Of[A], ct: ClassTag[A]) = {
    val encoder = DeriveJsonEncoder.gen[A]
    val decoder = DeriveJsonDecoder.gen[A]
    JsonCodec(encoder, decoder)
  }

object DeriveJsonEncoder extends AutoDerivation[JsonEncoder]:
  self =>

  override def join[A](ctx: CaseClass[Typeclass, A]): Typeclass[A] = (a: A) =>
    Json.Obj(
      ctx.params
        .foldLeft[ListMap[String, Json]](ListMap.empty) { case (chunk, param) =>
          val name  = param.label
          val value = param.typeclass.encode(param.deref(a))
          if (value == Json.Null) chunk
          else ListMap(name -> value) ++ chunk
        }
    )

  override def split[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] = (a: T) =>
    ctx.choose(a) { sub =>
      val value = sub.typeclass.encode(sub.cast(a))
      Json.Obj(
        ListMap(
          sub.typeInfo.short -> value
        )
      )
    }

  inline def gen[A](using mirror: Mirror.Of[A]) = self.derived[A]

object DeriveJsonDecoder:
  self =>

// TODO
//  override def split[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] = new JsonDecoder[T]:
//    override def decode(js: Json): Result[T] = ???
//
//  override def join[T](ctx: CaseClass[Typeclass, T]): Typeclass[T] = new JsonDecoder[T]:
//    override def decode(json: Json): Result[T] = ???

  inline def gen[A](using mirror: Mirror.Of[A], ct: ClassTag[A]) = JsonDecoder.derived[A]
