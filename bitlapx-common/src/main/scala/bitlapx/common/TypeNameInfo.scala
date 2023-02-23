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

import magnolia1.*

trait TypeNameInfo[T]:
  def name: TypeInfo
  def subtypeNames: Seq[TypeInfo]
end TypeNameInfo

object TypeNameInfo extends Derivation[TypeNameInfo]:
  def join[T](ctx: CaseClass[TypeNameInfo, T]): TypeNameInfo[T] =
    new TypeNameInfo[T]:
      def name: TypeInfo              = ctx.typeInfo
      def subtypeNames: Seq[TypeInfo] = Nil

  override def split[T](ctx: SealedTrait[TypeNameInfo, T]): TypeNameInfo[T] =
    new TypeNameInfo[T]:
      def name: TypeInfo              = ctx.typeInfo
      def subtypeNames: Seq[TypeInfo] = ctx.subtypes.map(_.typeInfo)

  given fallback[T]: TypeNameInfo[T] =
    new TypeNameInfo[T]:
      def name: TypeInfo              = TypeInfo("", "Unknown Type", Seq.empty)
      def subtypeNames: Seq[TypeInfo] = Nil