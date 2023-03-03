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

package bitlapx.common.function

import bitlapx.common.*
import bitlapx.common.mirror.FunctionMirror

import scala.quoted.*

/** @author
 *    梦境迷离
 *  @version 1.0,2023/3/3
 */
object FunctionMacro:

  private type IsFnArg[A <: FunctionArgument] = A

  def deriveMirror[Fn: Type](using Quotes): Expr[FunctionMirror[Fn]] =
    import quotes.reflect.*
    TypeRepr.of[Fn] match {
      case tpe @ AppliedType(_, tpeArgs) if tpe.isFunctionType =>
        val returnTpe = tpeArgs.last
        returnTpe.asType match {
          case '[ret] =>
            '{
              FunctionMirror.asInstanceOf[
                FunctionMirror[Fn] {
                  type Return = ret
                }
              ]
            }
        }

      case other =>
        report.errorAndAbort(s"FunctionMacro can only be created for function. Got ${other.show} instead.")
    }

  def refineFunctionArgument[Fn: Type, F[x <: FunctionArgument]: Type](
    function: Expr[Fn],
    initial: Expr[F[Nothing]]
  )(using Quotes) = {
    import quotes.reflect.*
    function.asTerm match {
      case func @ FunctionLambda(valDefs, _) =>
        refine(TypeRepr.of[FunctionArgument], valDefs).asType match {
          case '[IsFnArg[args]] => '{ $initial.asInstanceOf[F[args]] }
        }

      case other => report.errorAndAbort(s"Failed to extract named argument from ${other.show}")
    }
  }

  private def refine(using Quotes)(tpe: quotes.reflect.TypeRepr, valDefs: List[quotes.reflect.ValDef]) =
    import quotes.reflect.*
    valDefs.foldLeft(TypeRepr.of[FunctionArgument])((tpe, valDef) => Refinement(tpe, valDef.name, valDef.tpt.tpe))
