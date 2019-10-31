/*   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package sample

import razie.diesel.expr._
import razie.diesel.dom.RDOM._

object SampleExprParser {
  def main(args: Array[String]): Unit = {
   val input = "a + 2"
   val parser = new SimpleExprParser()
   // create and initialize context with "a"
   implicit val ctx: ECtx = new SimpleECtx().withP(P.fromTypedValue("a", 1))

   // parse and execute any expression
   val result = parser.parseExpr(input).map(_.applyTyped(""))

   println(result)
   }
}
