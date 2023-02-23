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

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.quoted.*

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/21
 */
object Debugger:

  inline def debug(inline exprs: Any*): Unit = ${ debugImpl('{ exprs }) }

  private def debugImpl(exprs: Expr[Seq[Any]])(using q: Quotes): Expr[Unit] =
    import q.reflect._

    def showWithValue(e: Expr[_]): Expr[String] = '{ ${ Expr(e.show) } + " = " + $e }

    val stringExps: Seq[Expr[String]] = exprs match
      case Varargs(es) =>
        es.map { e =>
          e.asTerm match {
            case Literal(c: Constant) => Expr(c.value.toString)
            case _                    => showWithValue(e)
          }
        }
      case e => List(showWithValue(e))

    val concatenatedStringsExp = stringExps.reduceOption((e1, e2) => '{ ${ e1 } + ", " + ${ e2 } }).getOrElse('{ "" })
    Bitlapx.printAtTime('{ "expr" }, concatenatedStringsExp)
