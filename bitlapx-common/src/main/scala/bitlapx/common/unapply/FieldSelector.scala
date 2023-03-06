package bitlapx.common.unapply

import scala.quoted.*

/** @author
 *    梦境迷离
 *  @version 1.0,2023/3/6
 */
object FieldSelector:
  def unapply(arg: Expr[Any])(using Quotes): Option[String] =
    import quotes.reflect.*
    PartialFunction.condOpt(arg.asTerm) { case Lambda(_, Select(Ident(_), fieldName)) =>
      fieldName
    }
end FieldSelector
