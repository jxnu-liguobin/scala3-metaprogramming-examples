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

import bitlapx.common.Failure.Suggestion
import bitlapx.common.field.Fields
import bitlapx.common.function.FunctionArgument
import bitlapx.common.unapply.*

import scala.quoted.*

/** @author
 *    梦境迷离
 *  @version 1.0,2023/3/3
 */
object Selectors:

  def fieldName[From: Type, FieldType](
    validFields: Fields,
    selector: Expr[From => FieldType]
  )(using Quotes): String =
    selector match {
      case FieldSelector(fieldName) if validFields.containsFieldWithName(fieldName) =>
        fieldName
      case other =>
        Failure.abort(Failure.InvalidFieldSelector(other, summon, Suggestion.fromFields(validFields)))
    }

  def argName[ArgType: Type, ArgSelector <: FunctionArgument](
    validArgs: Fields,
    selector: Expr[ArgSelector => ArgType]
  )(using Quotes): String =
    import quotes.reflect.*
    selector.asTerm match {
      case ArgSelector(argumentName) if validArgs.containsFieldWithName(argumentName) =>
        argumentName
      case ArgSelector(argumentName) =>
        Failure.abort(Failure.InvalidArgSelector.NotFound(selector, argumentName, Suggestion.fromFields(validArgs)))
      case other =>
        Failure.abort(Failure.InvalidArgSelector.NotAnArgSelector(selector, Suggestion.fromFields(validArgs)))
    }
