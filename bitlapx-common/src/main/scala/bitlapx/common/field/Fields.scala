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

package bitlapx.common.field

import bitlapx.common.function.FunctionArgument
import bitlapx.common.mirror.MaterializedMirror
import bitlapx.common.*

import scala.deriving.Mirror
import scala.quoted.*

/** @author
 *    梦境迷离
 *  @version 1.0,2023/3/3
 */
sealed trait Fields:
  export byName.{ apply => unsafeGet, contains => containsFieldWithName, get }

  val value: List[Field]

  val byName: Map[String, Field] = value.map(f => f.name -> f).toMap
end Fields

object Fields:
  def source(using sourceFields: Fields.Source): Fields.Source = sourceFields
  def dest(using destFields: Fields.Dest): Fields.Dest         = destFields

  final case class Source(value: List[Field]) extends Fields
  object Source                               extends FieldsCompanion[Source]

  final case class Dest(value: List[Field]) extends Fields
  object Dest                               extends FieldsCompanion[Dest]

  sealed abstract class FieldsCompanion[FieldsSubType <: Fields]:

    def apply(fields: List[Field]): FieldsSubType

    final def fromMirror[A: Type](mirror: Expr[Mirror.ProductOf[A]])(using Quotes): FieldsSubType =
      val materializedMirror = MaterializedMirror.createOrAbort(mirror)

      val fields = materializedMirror.mirroredElemLabels
        .zip(materializedMirror.mirroredElemTypes)
        .map((name, tpe) => Field(name, tpe.asType))
      apply(fields)

    end fromMirror

    final def fromFunctionArguments[ArgSelector <: FunctionArgument: Type](using Quotes): FieldsSubType =
      import quotes.reflect.*
      val fields = List.unfold(TypeRepr.of[ArgSelector]) { state =>
        PartialFunction.condOpt(state) { case Refinement(parent, name, fieldTpe) =>
          Field(name, fieldTpe.asType) -> parent
        }
      }
      apply(fields.reverse)
    end fromFunctionArguments

    final def fromValDefs(using Quotes)(valDefs: List[quotes.reflect.ValDef]): FieldsSubType =
      val fields = valDefs.map(vd => Field(vd.name, vd.tpt.tpe.asType))
      apply(fields)
    end fromValDefs

  end FieldsCompanion
