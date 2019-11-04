/*  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import razie.diesel.dom.RDOM.P
import razie.diesel.dom.{RDOM, WTypes}
import razie.diesel.expr.{BCMP2, ECtx}
import razie.wiki.Enc

/** utilities for nodes */
package object nodes {

  type Attrs = List[RDOM.P]
  type MatchAttrs = List[RDOM.PM]

  /** check if it matches a regex */
  def regexm(re:String, str:String) = {
    if((re startsWith "/") && (re endsWith "/")) {
      str.matches(re.substring(1,re.length-1))
    } else
      false
  }

  /** check to match the arguments */
  def sketchAttrs(defs:MatchAttrs, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) : Attrs = {
    defs.map{p=>
      val v = if(p.dflt.length > 0) p.dflt else p.expr.map(_.apply("")).mkString
      P(p.name, v, p.ttype)
    }
  }

  /** prep for display */
  def htmlValue(s:String) = Enc.escapeHtml(s.take(100))

  def flattenJson (p: P)(implicit ctx: ECtx) : Attrs = {
    val v = p.calculatedTypedValue
    assert(v.contentType == WTypes.JSON, "input needs to be JSON, but it's: "+p)
    val j = p.calculatedTypedValue.asJson
    j.map{t=>
      P.fromTypedValue(t._1, t._2)
    }.toList
  }

  /**
   * matching attrs
    *
    * @param in
    * @param cond
    * @param cole collector for match/fail
    * @param foundName opt function to mark value in collector
    * @param positive - false if the condition is negative, like filterNot
   *
   * (a,b,c) they occur in whatever sequence
   *
   * (1,b,c) it occurs in position with value
   *
   * (a=1) it occurs with value
   */
  def testA(in: Attrs,
            cond: MatchAttrs,
            cole: Option[MatchCollector] = None,
            foundName:Option[RDOM.P => Unit]=None,
            positive:Boolean = true)(implicit ctx: ECtx): Boolean = {
    // for each match

      val result = cond.zipWithIndex.foldLeft(true)((a, b) => a && {
        var res = false

        val pm = b._1

        // testing for name and value
        if (pm.isMatch && (pm.dflt.size > 0 || pm.expr.isDefined)) {
          if (b._1.name.size > 0) {
            res = in.exists(x => pm.check(x)) || ctx.exists(x => pm.check(x))

            if (!res && positive || res && !positive) in.find(_.name == pm.name).map { p =>
              // mark it in the cole
              foundName.map(_.apply(p))
            }

            if (!res) {
              // last try: any value in context
              val bres = new BCMP2(pm.ident, pm.op, pm.valExpr).bapply("")
              res = bres.value

              if (!res) { // failed to find any valid exprs, just evaluate the left side to get some info
//                if (positive) cole.map(_.missedValue(AExprIdent(pm.name).applyTyped("")))
                if (positive && bres.a.isDefined) cole.map(_.missedValue(bres.a.get))
              } else {
                // for regex matches, use each capture group and set as parm in context
                if (pm.op == "~=") {
                  // extract parms
                  val a = pm.ident.apply("")
                  val b = pm.valExpr.apply("")
                  val groups = EContent.extractRegexParms(b.toString, a.toString)

                  groups.foreach(t => ctx.put(P(t._1, t._2)))
                }
              }
            }

            if (res && positive) cole.map(_.plus(pm.name + pm.op + pm.dflt))
            else cole.map(_.minus(pm.name, in.find(_.name == pm.name).mkString, pm))
          }
        } else if(pm.isMatch) {
          // test just the name (presence): check and record the name failure
          if (pm.name.size > 0) {
            // I don't include the context - this leads to side-effects, just use an IF after the match...
            res = in.exists(_.name == pm.name) // || ctx.exists(_.name == b._1.name)

            if (res && positive) cole.map(_.plus(pm.name))
            else cole.map(_.minus(pm.name, pm.name, pm))
          }
        } else {
          val bres = pm.checkAsCond()
          res = bres.value

          // todo nice to extract a parm that didn't match from the pm.BExpr and report it
          if (res && positive) cole.map(_.plus(""))
          else cole.map(_.minus(bres.a.map(_.name).mkString, bres.a.map(_.calculatedValue).mkString, pm))
        }
        res
      })

      if (positive) result else !result
  }

  def toHtmlAttrs(attrs: Attrs)      = if(attrs.nonEmpty) s"""${attrs.map(_.toHtml).mkString("(", ", ", ")")}""" else ""
  def toHtmlMAttrs(attrs: MatchAttrs) = if(attrs.nonEmpty) s"""${attrs.map(_.toHtml).mkString("(", ", ", ")")}""" else ""
  def toHtmlPAttrs(attrs: List[PAS]) = if(attrs.nonEmpty) s"""${attrs.map(_.toHtml).mkString("(", ", ", ")")}""" else ""

  //todo when types are supported, remove this method and all its uses
  def stripQuotes(s:String) =
    if(s.startsWith("\"") && s.endsWith("\"")) s.substring(1,s.length-1) else s

}
