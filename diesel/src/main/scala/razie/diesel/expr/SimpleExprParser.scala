/*   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.expr

/** A simple parser for our simple specs
  *
  * DomParser is the actual Diesel/Dom parser.
  * We extend from it to include its functionality and then we add its parsing rules with withBlocks()
  */
class SimpleExprParser extends ExprParser {

  def parseExpr (input: String):Option[Expr] = {
    parseAll(expr, input) match {
      case Success(value, _) => Some(value)
      case NoSuccess(msg, next) => None
        //todo ? throw new DieselExprException("Parsing error: " + msg)
    }
  }

  def parseIdent (input: String):Option[AExprIdent] = {
    parseAll(aidentExpr, input) match {
      case Success(value, _) => Some(value)
      case NoSuccess(msg, next) => None
      //todo ? throw new DieselExprException("Parsing error: " + msg)
    }
  }

  def parseCond (input: String):Option[BoolExpr] = {
    parseAll(cond, input) match {
      case Success(value, _) => Some(value)
      case NoSuccess(msg, next) => None
      //todo ? throw new DieselExprException("Parsing error: " + msg)
    }
  }

}

