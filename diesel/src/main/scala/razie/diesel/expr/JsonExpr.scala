/*   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.expr

import org.json.JSONObject
import razie.diesel.dom.RDOM.{P, PValue}
import razie.diesel.dom._
import razie.diesel.engine.exec.EESnakk

/** a json document block:
  * - you may or may not use quotes for names
  * -
  */
case class JBlockExpr(ex: List[(String, Expr)], schema:Option[String]=None) extends Expr {
  val expr = "{" + ex.map(t=>t._1 + ":" + t._2.toString).mkString(",") + "}"

  override def apply(v: Any)(implicit ctx: ECtx) = applyTyped(v).currentStringValue

  override def applyTyped(v: Any)(implicit ctx: ECtx) = {
    // todo this can be way faster for a few types, like Array - $send ctx.set (state281 = {source: [0,1,2,3,4,5], dest: [], aux: []})
//    val orig = template(expr)
    val orig = ex
      .map(t=> (t._1, t._2.applyTyped(v)))
      .map(t=> (t._1, t._2 match {
        case p@P(n,d,WTypes.wt.NUMBER, _, _, Some(PValue(i:Int, _))) => i
        case p@P(n,d,WTypes.wt.NUMBER, _, _, Some(PValue(i:Long, _))) => i
        case p@P(n,d,WTypes.wt.NUMBER, _, _, Some(PValue(i:Double, _))) => i

        case p@P(n,d,WTypes.wt.BOOLEAN, _, _, Some(PValue(b:Boolean, _))) => b

        case p:P => p.currentStringValue match {
          case i: String if i.trim.startsWith("[") && i.trim.endsWith("]") => i
          case i: String if i.trim.startsWith("{") && i.trim.endsWith("}") => i
          case i => "\"" + i + "\""
        }

      }))
      .map(t=> s""" "${t._1}" : ${t._2} """)
      .mkString(",")

    // parse and clean it up so it blows up right here if invalid
    val j = new JSONObject(s"{$orig}")
    P.fromTypedValue("", j, getType)
  }

  override val getType: WType = schema.map(WTypes.wt.JSON.withSchema).getOrElse(WTypes.wt.JSON)

  // replace ${e} with value
  def template(s: String)(implicit ctx: ECtx) = {

    EESnakk.prepStr2(s, Nil)
  }
}

/** a json array */
case class JArrExpr(ex: List[Expr]) extends Expr {
  val expr = "[" + ex.mkString(",") + "]"

  private def calculate(v: Any)(implicit ctx: ECtx) = {
    //    val orig = template(expr)
    val orig = ex.map{e=>
      val p = e.applyTyped(v)
      p.calculatedTypedValue.asEscapedJSString
    }.mkString(",")

    // parse and clean it up so it blows up right here if invalid
    new org.json.JSONArray(s"[$orig]")
  }

  override def apply(v: Any)(implicit ctx: ECtx) = {
    calculate(v).toString
  }

  override def applyTyped(v: Any)(implicit ctx: ECtx): P = {
    val orig = ex.map(_.apply(v)).mkString(",")
    val ja = calculate(v)
    P.fromTypedValue("", ja)
  }

  override def getType: WType = WTypes.wt.ARRAY

  // replace ${e} with value
  def template(s: String)(implicit ctx: ECtx) = {

    EESnakk.prepStr2(s, Nil)
  }

}


