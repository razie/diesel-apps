package razie.diesel

import razie.diesel.dom._
import razie.diesel.dom.RDOM.{P, PM}
import razie.diesel.engine.EContent
import razie.diesel.expr.{AExprIdent, BCMP2}
import razie.tconf.EPos
import razie.wiki.Enc
import razie.wiki.parser.PAS
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * simple, neutral domain model representation: class/object/function
 *
 * These are collected in RDomain
 */
package object ext {

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
      P(p.name, v, p.ttype, p.ref, p.multi)
    }
  }

  /** prep for display */
  def htmlValue(s:String) = Enc.escapeHtml(s.take(100))

  /** a single match, collected when looking for expectations */
  class SingleMatch(val x: Any) {
    var score = 0;
    val diffs = new mutable.HashMap[String, (Any, Any)]() // (found, expected)
    val misses = new mutable.ArrayBuffer[String] // names didn't match

    def plus(s: String) = {
      score += 1
    }

    def minus(name: String, found: Any, expected:Any) = {
      diffs.put(name, (found.toString, expected.toString))
    }

    // missed opportunity to match this
    def missed(name: String) = {
      misses.append(name)
    }

    // missed opportunity to match this
    def missedValue(p: P) = {
      misses.append(p.toString)
    }
  }

  /** collects the intermediary tests for a match, when looknig for expectations */
  class MatchCollector {
    var cur = new SingleMatch("")
    var highestScore = 0;
    var highestMatching: Option[SingleMatch] = None
    val old = new ListBuffer[SingleMatch]()

    def done = {
      if (cur.score >= highestScore) {
        highestScore = cur.score
        highestMatching = Some(cur)
      }
      old.append(cur)
    }

    def newMatch(x: Any) = {
      done
      cur = new SingleMatch(x)
    }

    def plus(s: String) = cur.plus(s)

    def minus(name: String, found: Any, expected:Any) =
      cur.minus(name, found, expected)

    // record missed opportunity to match (names did not match)
    def missed(name: String) =
      cur.missed(name)

    // record missed opportunity to match (names did not match)
    def missedValue(p:P) =
      cur.missedValue(p)

    def toHtml =
      highestMatching
        .map {h=>
          h.diffs.values.map(_._1)
            .toList
            .map(x => s"""<span style="color:red">${htmlValue(x.toString)}</span>""")
            .mkString(",") +
          (
            // todo list them in order of close to far (i.e. a close name)
            if(h.misses.size > 0)
              "found: " + h.misses
              .map(x => s"""<span style="color:red">${htmlValue(x)}</span>""")
              .mkString(",")
            else ""
          )
        }
        .mkString
  }

  def check (in:P, pm:PM)(implicit ctx: ECtx) = {
    // match simple names - look at testA for complex evaluators
    in.name == pm.name && pm.ident.rest.isEmpty && {
      val r = new BCMP2(in.valExpr, pm.op, pm.valExpr).apply("")

      // for regex matches, use each capture group and set as parm in context
      if(pm.op == "~=") {
        // extract parms
        val a = in.valExpr.apply("")
        val b = pm.valExpr.apply("")
        val groups = EContent.extractRegexParms(b.toString, a.toString)

        groups.foreach(t=> ctx.put(P(t._1, t._2)))
      }

      if(! r) {
//         name found but no value match - mark the name
      }
      r
    }
  }

  /**
   * matching attrs
    *
    * @param in
    * @param cond
    * @param cole collector for match/fail
    * @param foundName opt function to mark value in collector
   *
   * (a,b,c) they occur in whatever sequence
   *
   * (1,b,c) it occurs in position with value
   *
   * (a=1) it occurs with value
   *
   */
  def testA(in: Attrs,
            cond: MatchAttrs,
            cole: Option[MatchCollector] = None,
            foundName:Option[RDOM.P => Unit]=None)(implicit ctx: ECtx) = {
    // for each match
    cond.zipWithIndex.foldLeft(true)((a, b) => a && {
      var res = false

      val pm = b._1

      // testing for name and value
      if (b._1.dflt.size > 0 || b._1.expr.isDefined) {
        if (b._1.name.size > 0) {
          res = in.exists(x => check(x, b._1)) || ctx.exists(x => check(x, b._1))

          if(!res) in.find(_.name == b._1.name).map {p=>
            // mark it in the cole
            foundName.map(_.apply(p))
          }

          if(!res) {
            // last try: any value in context
            res = new BCMP2(pm.ident, pm.op, pm.valExpr).apply("")

            if(!res) // failed to find any valid exprs, just evaluate the left side to get some info
              cole.map(_.missedValue(AExprIdent(pm.name).applyTyped("")))
            else {
              // for regex matches, use each capture group and set as parm in context
              if(pm.op == "~=") {
                // extract parms
                val a = pm.ident.apply("")
                val b = pm.valExpr.apply("")
                val groups = EContent.extractRegexParms(b.toString, a.toString)

                groups.foreach(t=> ctx.put(P(t._1, t._2)))
              }
            }
          }

          if (res) cole.map(_.plus(b._1.name + b._1.op + b._1.dflt))
          else cole.map(_.minus(b._1.name, in.find(_.name == b._1.name).mkString, b._1))
        }
      } else {
        // test just the name (presence): check and record the name failure
        if (b._1.name.size > 0) {
          // I don't include the context - this leads to side-effects, just use an IF after the match...
          res = in.exists(_.name == b._1.name) // || ctx.exists(_.name == b._1.name)

          if (res) cole.map(_.plus(b._1.name))
          else cole.map(_.minus(b._1.name, b._1.name, b._1))
        }
      }
      res
    })
  }

  trait HasPosition {
    def pos : Option[EPos]

    /** key span with possible link. pass None to not have a link */
    def kspan(s: String, k: String = "default", overwritePos:Option[EPos] = Some(EPos.EMPTY), title:Option[String]=None, kind:Option[String]=None) = {
      val actualPos = if(overwritePos.exists(_.isEmpty)) pos else overwritePos
      def mkref: String = actualPos.map(_.toRef).mkString
      val t = title.map(CanHtml.prepTitle)
      val kin = kind.map(k=> s"""kind="${k}"""").mkString

      actualPos.map(p =>
        s"""<span $kin posw="${p.wpath}" posr="${p.line}" onclick="$mkref" style="cursor:pointer" class="label label-$k" ${t.mkString}>$s</span>&nbsp;"""
      ) getOrElse
        s"""<span $kin class="label label-$k" ${t.mkString}>$s</span>&nbsp;"""
    }
  }

  object CanHtml {
    def prepTitle(title:String) = {
      val h = Enc.escapeHtml(title)
      val x = h.replaceAll("\\\"", "")
//      val x = h.replaceAll("\\\"", "\\\"")
      val t = if(h.length > 0) s"""title="$x" """ else ""
      t
    }

    def span(s: String, k: String = "default", title:String="") = {
      val t = prepTitle(title)
      s"""<span class="label label-$k" $t>$s</span>"""
    }
  }

  /** instances have an toHtml method */
  trait CanHtml {
    /** format an html keyword span
      *
      * @param s the keyword
      * @param k the color code
      * @param title optional hover title
      * @param extra optional other attrs
      * @return
      */
    def span(s: String, k: String = "default", title:String="", extra:String="") = {
      val t = CanHtml.prepTitle(title)
      s"""<span class="label label-$k" $t $extra>$s</span>"""
    }

    /** format a clickable span, which dumps content
      *
      * @param s
      * @param k
      * @param title
      * @param extra
      * @return
      */
    def spanClick(s: String, k: String = "default", title:String="", extra:String="") = {
      val id = java.util.UUID.randomUUID().toString
      span(
        s,
        k,
        title,
        s"""style="cursor:help" id="$id" onclick="dieselNodeLog($$('#$id').prop('title'));""""
      ) + " " + extra
    }

    /**
      * format an html message span
      *
      * wrap for EMsg where the kspan will wrap it anyways
      */
    def ea(e: String, a: String, title:String="", wrap:Boolean=true) = {
      val t = CanHtml.prepTitle(title)
      (if(wrap) s"""<span class="label label-default" $t>""" else "") +
         s"""<span style="font-weight:bold; color:moccasin">$e</span>.<span
         |      class="" style="font-weight:bold; color:lightblue">$a</span>""".stripMargin +
       (if(wrap)  """ </span>""" else "")
    }

    /** *
      * format an html element span
      */
    def token(s: String, title:String="", extra:String="") = {
      val t = CanHtml.prepTitle(title)
      s"""<span $t $extra>$s</span>"""
    }

    def tokenValue(s: String) =
      "<code>"+token (s, "value", """ class="string" """)+"</code>"

    def toHtml: String
  }

  def toHtmlAttrs(attrs: Attrs)      = if(attrs.nonEmpty) s"""${attrs.map(_.toHtml).mkString("(", ", ", ")")}""" else ""
  def toHtmlMAttrs(attrs: MatchAttrs) = if(attrs.nonEmpty) s"""${attrs.map(_.toHtml).mkString("(", ", ", ")")}""" else ""
  def toHtmlPAttrs(attrs: List[PAS]) = if(attrs.nonEmpty) s"""${attrs.map(_.toHtml).mkString("(", ", ", ")")}""" else ""

  //todo when types are supported, remove this method and all its uses
  def stripQuotes(s:String) =
    if(s.startsWith("\"") && s.endsWith("\"")) s.substring(1,s.length-1) else s

}

