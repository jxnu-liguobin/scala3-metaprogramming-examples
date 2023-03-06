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
import scala.deriving.*
import scala.deriving.Mirror
import scala.quoted.*

object CollectAnnotations:
  def paramAnns[T: Type](using Quotes): Expr[List[(String, List[Any])]] =
    new CollectAnnotations[T].paramAnns

end CollectAnnotations

final class CollectAnnotations[T: Type](using val quotes: Quotes):

  import quotes.reflect.*

  private val tpe: TypeRepr = TypeRepr.of[T]

  def anns: Expr[List[Any]] =
    Expr.ofList {
      tpe.typeSymbol.annotations
        .filter(filterAnnotation)
        .map(_.asExpr.asInstanceOf[Expr[Any]])
    }

  def inheritedAnns: Expr[List[Any]] =
    Expr.ofList {
      tpe.baseClasses
        .filterNot(isObjectOrScala)
        .collect {
          case s if s != tpe.typeSymbol => s.annotations
        } // skip self
        .flatten
        .filter(filterAnnotation)
        .map(_.asExpr.asInstanceOf[Expr[Any]])
    }

  def typeAnns: Expr[List[Any]] = {

    def getAnnotations(t: TypeRepr): List[Term] = t match
      case AnnotatedType(inner, ann) => ann :: getAnnotations(inner)
      case _                         => Nil

    tpe.typeSymbol.tree match
      case ClassDef(_, _, parents, _, _) =>
        Expr.ofList {
          parents.collect { case t: TypeTree => t.tpe }
            .flatMap(getAnnotations)
            .filter(filterAnnotation)
            .map(_.asExpr.asInstanceOf[Expr[Any]])
        }
      case _ => Expr.ofList(List.empty)
  }

  def paramAnns: Expr[List[(String, List[Any])]] =
    Expr.ofList {
      groupByParamName {
        (fromConstructor(tpe.typeSymbol) ++ fromDeclarations(tpe.typeSymbol)).filter { case (_, anns) => anns.nonEmpty }
      }
    }

  def inheritedParamAnns: Expr[List[(String, List[Any])]] =
    Expr.ofList {
      groupByParamName {
        tpe.baseClasses
          .filterNot(isObjectOrScala)
          .collect {
            case s if s != tpe.typeSymbol =>
              (fromConstructor(s) ++ fromDeclarations(s)).filter { case (_, anns) =>
                anns.nonEmpty
              }
          }
          .flatten
      }
    }

  private def fromConstructor(from: Symbol): List[(String, List[Expr[Any]])] =
    from.primaryConstructor.paramSymss.flatten.map { field =>
      field.name -> field.annotations
        .filter(filterAnnotation)
        .map(_.asExpr.asInstanceOf[Expr[Any]])
    }

  private def fromDeclarations(
    from: Symbol
  ): List[(String, List[Expr[Any]])] =
    from.declarations.collect { case field: Symbol =>
      field.tree match
        case v: ValDef =>
          field.name -> field.annotations
            .filter(filterAnnotation)
            .map(_.asExpr.asInstanceOf[Expr[Any]])
    }

  private def groupByParamName(anns: List[(String, List[Expr[Any]])]) =
    anns.groupBy { case (name, _) => name }.toList.map { case (name, l) => name -> l.flatMap(_._2) }.map {
      (name, anns) => Expr.ofTuple(Expr(name), Expr.ofList(anns))
    }

  private def isObjectOrScala(bc: Symbol) =
    bc.name.contains("java.lang.Object") || bc.fullName.startsWith("scala.")

  private def filterAnnotation(a: Term): Boolean =
    a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
    a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"
