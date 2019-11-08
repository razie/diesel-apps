/*   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.expr

import razie.diesel.dom.RDOM.P
import razie.diesel.dom._

/** a function */
case class CExprNull() extends Expr {
  override def getType: WType = WTypes.wt.UNDEFINED
  override def expr = "null"

  override def apply(v: Any)(implicit ctx: ECtx) = applyTyped(v).calculatedValue

  override def applyTyped(v: Any)(implicit ctx: ECtx): P = P("", "", WTypes.wt.UNDEFINED)

  override def toDsl = expr
  override def toHtml = expr
}


/**
  * constant expression - similar to PValue
  */
case class CExpr[T](ee: T, ttype: WType = WTypes.wt.EMPTY) extends Expr {
  val expr = ee.toString

  override def apply(v: Any)(implicit ctx: ECtx) = applyTyped(v).currentStringValue

  override def applyTyped(v: Any)(implicit ctx: ECtx): P = {
    val es = P.asString(ee)

    if (ttype == WTypes.NUMBER) {
      if (es.contains("."))
        P("", es, ttype).withValue(es.toDouble, WTypes.wt.NUMBER)
      else
        P("", es, ttype).withValue(es.toLong, WTypes.wt.NUMBER)
    } else if (ttype == WTypes.BOOLEAN) {
      P("", es, ttype).withValue(es.toBoolean, WTypes.wt.BOOLEAN)
    } else {
      // expand templates by default
      if (es contains "${") {
        var s1 = ""
        try {
          val PAT = """\$\{([^\}]*)\}""".r
          val eeEscaped = es
            .replaceAllLiterally("(", """\(""")
            .replaceAllLiterally(")", """\)""")
          s1 = PAT.replaceAllIn(es, {
            m =>
              (new SimpleExprParser).parseExpr(m.group(1)).map { e =>
                val res = P("x", "", WTypes.wt.EMPTY, Some(e)).calculatedValue

                // if the parm's value contains $ it would not be expanded - that's enough, eh?
                // todo recursive expansions?
                var e1 = res
                    .replaceAllLiterally("\\", "\\\\") // escape escaped
                    .replaceAll("\\$", "\\\\\\$")
                // should not escape double quotes all the time !!
//                e1 = e1
//                    .replaceAll("^\"", "\\\\\"") // escape double quotes
//                e1 = e1
//                    .replaceAll("""([^\\])"""", "$1\\\\\"") // escape unescaped double quotes

                e1
              } getOrElse
                s"{ERROR: ${m.group(1)}"
          })
        } catch {
          case e: Exception =>
            throw new DieselExprException(s"REGEX err for $es - " + e.getMessage)
              .initCause(e)
        }
        P("", s1, ttype)
      } else
        P("", es, ttype).withCachedValue(ee, ttype, es)
    }
  }

  override def toDsl = if (ttype == "String") ("\"" + expr + "\"") else expr
  override def getType: WType = ttype
  override def toHtml = tokenValue(escapeHtml(toDsl))
}



