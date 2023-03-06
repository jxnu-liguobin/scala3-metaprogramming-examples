package bitlapx.common.unapply

import bitlapx.common.function.FunctionArgumentSelector

import scala.quoted.*

/** @author
 *    梦境迷离
 *  @version 1.0,2023/3/6
 */
object ArgSelector:

  def unapply(using Quotes)(arg: quotes.reflect.Term): Option[String] =
    import quotes.reflect.*
    PartialFunction.condOpt(arg) { case Lambda(_, FunctionArgumentSelector(argumentName)) =>
      argumentName
    }

end ArgSelector
