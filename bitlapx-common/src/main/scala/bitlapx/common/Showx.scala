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

import scala.compiletime.*
import scala.deriving.Mirror

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/21
 */
trait Showx[T]:
  def show(t: T): String
end Showx

object Showx:

  given showAny[T]: Showx[T] = (t: T) => t.toString

  inline given derived[T](using m: Mirror.Of[T]): Showx[T] =
    inline m match {
      case s: Mirror.SumOf[T] => ???
      case p: Mirror.ProductOf[T] =>
        val showElems: List[Showx[_]] = summonShowAll[m.MirroredElemTypes]
        showProduct(p, showElems)
    }

  private inline def summonShowAll[T <: Tuple]: List[Showx[_]] = inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => summonInline[Showx[t]] :: summonShowAll[ts]
  }

  private inline def showProduct[T](p: Mirror.ProductOf[T], showElems: List[Showx[_]]): Showx[T] = {
    val label      = constValue[p.MirroredLabel]
    val elemLabels = labelsToList[p.MirroredElemLabels]
    (t: T) => {
      val elemValues = t.asInstanceOf[Product].productIterator.toList
      val elems = elemLabels
        .zip(elemValues)
        .zip(showElems)
        .map { case ((elLabel, el), safeShow) =>
          val shown = if (isSensitive(elLabel)) "***" else safeShow.asInstanceOf[Showx[Any]].show(el)
          elLabel + "=" + shown
        }
        .mkString(",")
      s"$label($elems)"
    }
  }

  private inline def labelsToList[T <: Tuple]: List[String] = inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => constValue[t].toString :: labelsToList[ts]
  }

  private def isSensitive(n: String): Boolean = {
    val nn = n.toLowerCase
    nn.contains("token") || nn.contains("apikey") || nn.contains("password")
  }

end Showx
