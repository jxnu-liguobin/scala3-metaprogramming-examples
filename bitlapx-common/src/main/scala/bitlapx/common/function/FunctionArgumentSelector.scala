package bitlapx.common.function

import scala.quoted.*

/** @author
 *    梦境迷离
 *  @version 1.0,2023/3/6
 */
object FunctionArgumentSelector:

  def unapply(using Quotes)(arg: quotes.reflect.Term): Option[String] =
    PartialFunction.condOpt(arg.asExpr) {
      case '{
            type argSelector <: FunctionArgument;
            ($args: `argSelector`).selectDynamic($selectedArg).$asInstanceOf$[tpe]
          } =>
        selectedArg.valueOrAbort

    }
end FunctionArgumentSelector
