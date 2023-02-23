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

import scala.quoted.*
import scala.annotation.experimental

/** @author
 *    梦境迷离
 *  @version 1.0,2023/2/21
 */
object Client:

  transparent inline def client[T, R](r: () => R): T = ${ Client.clientImpl[T, R]('{ r }) }

  private def isExpectedReturnType[R: Type](using Quotes): quotes.reflect.Symbol => Boolean = { method =>
    import quotes.reflect.*

    val expectedReturnType = TypeRepr.of[R]

    method.tree match {
      case DefDef(_, _, typedTree, _) =>
        TypeRepr.of(using typedTree.tpe.asType) <:< expectedReturnType
      case _ => false
    }
  }

  private def checkMethod[R: Type](using q: Quotes)(method: quotes.reflect.Symbol): Option[String] = {
    val isExpectedReturnTypeFun = isExpectedReturnType[R]

    Option.when(method.paramSymss.headOption.exists(_.exists(_.isType)))(
      s"Method ${method.name} has a generic type parameter, this is not supported"
    ) orElse
      Option.when(!isExpectedReturnTypeFun(method))(s"Method ${method.name} has unexpected return type")
  }

  private def definedMethodsInType[T: Type](using Quotes): List[quotes.reflect.Symbol] = {
    import quotes.reflect.*

    val tree = TypeTree.of[T]

    for {
      member <- tree.symbol.methodMembers
      if member.flags.is(Flags.Deferred)
      if !member.flags.is(Flags.Private)
      if !member.flags.is(Flags.Protected)
      if !member.flags.is(Flags.PrivateLocal)
      if !member.isClassConstructor
      if !member.flags.is(Flags.Synthetic)
    } yield member
  }

  @experimental
  def clientImpl[T: Type, R: Type](r: Expr[() => R])(using Quotes): Expr[T] = {
    import quotes.reflect.*

    val methods        = definedMethodsInType[T]
    val invalidMethods = methods.flatMap(checkMethod[R])
    if (invalidMethods.nonEmpty) {
      report.errorAndAbort(s"Invalid methods: ${invalidMethods.mkString(", ")}")
    }

    val className = "_Anon"
    val parents   = List(TypeTree.of[Object], TypeTree.of[T])

    def decls(cls: Symbol): List[Symbol] = methods.map { method =>
      method.tree.changeOwner(cls) match {
        case DefDef(name, clauses, typedTree, _) =>
          val tpeRepr = TypeRepr.of(using typedTree.tpe.asType)
          val names   = clauses.flatMap(_.params.collect { case v: ValDef => v.name })
          val tpes    = clauses.flatMap(_.params.collect { case v: ValDef => v.tpt.tpe })

          val methodType = if (clauses.isEmpty) ByNameType(tpeRepr) else MethodType(names)(_ => tpes, _ => tpeRepr)
          Symbol.newMethod(
            cls,
            name,
            methodType,
            flags = Flags.EmptyFlags,
            privateWithin = method.privateWithin.fold(Symbol.noSymbol)(_.typeSymbol)
          )
        case _ =>
          report.errorAndAbort(s"Cannot detect type of method: ${method.name}")
      }
    }

    val cls    = Symbol.newClass(Symbol.spliceOwner, className, parents.map(_.tpe), decls, selfType = None)
    val body   = cls.declaredMethods.map(method => DefDef(method, argss => Some('{ ${ r }.apply() }.asTerm)))
    val clsDef = ClassDef(cls, parents, body)
    val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[T])
    Block(List(clsDef), newCls).asExprOf[T]
  }
