/**
 *  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.ext

import razie.diesel.dom.RDOM._
import razie.diesel.dom._
import razie.diesel.engine.DomAstInfo
import razie.diesel.exec.Executors
import razie.diesel.expr.AExprIdent
import razie.tconf.EPos
import razie.wiki.Enc
import razie.wiki.parser.PAS
import scala.Option.option2Iterable

/** simple assignment - needed because the left side is more than just a val
  */
case class EMsgPas(attrs: List[PAS]=Nil) extends CanHtml with HasPosition with DomAstInfo {

  /** the pos of the rule that decomposes me, as a spec */
  var rulePos: Option[EPos] = None

  /** the pos of the rule/map that generated me as an instance */
  var pos : Option[EPos] = None

  def withRulePos(p:Option[EPos]) = {this.rulePos = p; this}
  def withPos(p:Option[EPos]) = {this.pos = p; this}

  private def msgLabelColor: String = "primary"

  // if this was an instance and you know of a spec
  private def first(instPos:Option[EPos]) : String = {
    // clean visual stypes annotations
    val stypeStr = "".replaceAllLiterally(",prune", "").replaceAllLiterally(",warn", "")
    kspan("msg", msgLabelColor, instPos) + span(stypeStr, "info") + (if (stypeStr.trim.length > 0) " " else "")
    //    kspan("msg", msgLabelColor, spec.flatMap(_.pos)) + span(stypeStr, "info") + (if(stypeStr.trim.length > 0) " " else "")
    }

  /** this html works well in a diesel fiddle, use toHtmlInPage elsewhere */
  override def toHtml = {
    /*span(arch+"::")+*/first(pos) + " " + toHtmlPAttrs(attrs)
  }

  override def toString =
    s""" (${attrs.mkString(", ")})"""

  override def shouldPrune = false
  override def shouldIgnore = false
  override def shouldSkip = false
  override def shouldRollup = true

}

/** a message
  *
  * @param entity
  * @param met
  * @param attrs
  * @param arch
  * @param ret
  */
case class EMsg(
                 entity: String,
                 met: String,
                 attrs: List[RDOM.P]=Nil,
                 arch:String="",
                 ret: List[RDOM.P] = Nil,
                 stype: String = "") extends CanHtml with HasPosition with DomAstInfo {

  import EMsg._

  /** my specification - has attributes like public etc */
  var spec: Option[EMsg] = None

  /** the pos of the rule that decomposes me, as a spec */
  var rulePos: Option[EPos] = None

  /** the pos of the rule/map that generated me as an instance */
  var pos : Option[EPos] = None

  def withSpec(p:Option[EMsg]) = {this.spec = p; this}
  def withRulePos(p:Option[EPos]) = {this.rulePos = p; this}
  def withPos(p:Option[EPos]) = {this.pos = p; this}

  def asCtx (implicit ctx:ECtx) : ECtx = new StaticECtx(this.attrs, Some(ctx))

  /** copy doesn't copy over the vars */
  def copiedFrom (from:EMsg) =  {
    this.spec = from.spec
    this.pos = from.pos
    this.rulePos = from.rulePos
    this
  }

  def toj : Map[String,Any] =
    Map (
      "class" -> "EMsg",
      "arch" -> arch,
      "entity" -> entity,
      "met" -> met,
      "attrs" -> attrs.map { p =>
        Map(
          "name" -> p.name,
          "value" -> p.currentStringValue
        )
      },
      "ret" -> ret.map { p =>
        Map(
          "name" -> p.name,
          "value" -> p.currentStringValue
        )
      },
      "stype" -> stype
    ) ++
      pos.map{p=>
        Map ("ref" -> p.toRef,
          "pos" -> p.toJmap
        )
      }.getOrElse(Map.empty) ++
      rulePos.map{p=>
        Map ("ref" -> p.toRef,
          "pos" -> p.toJmap
        )
      }.getOrElse(Map.empty)

  // if this was an instance and you know of a spec
  private def first(instPos:Option[EPos]) : String =
    spec
      .filter(x=> !x.equals(this)) // avoid stackoverflow if self is spec
      .map(_.first(instPos))
      .getOrElse {
        // clean visual stypes annotations
        val stypeStr = stype.replaceAllLiterally(",prune", "").replaceAllLiterally(",warn", "")
        kspan("msg", msgLabelColor, instPos) + span(stypeStr, "info") + (if (stypeStr.trim.length > 0) " " else "")
        //    kspan("msg", msgLabelColor, spec.flatMap(_.pos)) + span(stypeStr, "info") + (if(stypeStr.trim.length > 0) " " else "")
      }

  /** find the spec and get its pos */
  private def specPos: Option[EPos] = {
    // here is where you decide what to show: $msg (spec) or $when (rulePos)
    rulePos orElse spec.flatMap(_.pos)
  }

  /** if has executor */
  def hasExecutor:Boolean = Executors.withAll(_.exists(_.test(this)(ECtx.empty)))

  def ea:String = entity + "." + met

  def isResolved:Boolean =
    if(spec.filter(x=> !x.equals(this)).exists(_.isResolved)) true
    else (
      if ((stype contains "GET") || (stype contains "POST") || hasExecutor || entity == "diesel") true
      else false
  )

  /** color - if has executor */
  private def msgLabelColor: String =
    if (stype contains WARNING) "warning"
    else if(isResolved) "default"
    else "primary"

  /** extract a match from this message signature */
  def asMatch = EMatch(entity, met, attrs.filter(p=> p.dflt != "" || p.expr.isDefined).map {p=>
    PM (AExprIdent(p.name), p.ttype, "==", p.dflt, p.expr)
  })

  /** message name as a nice link to spec as well */
  private def eaHtml =
    kspan(ea(entity,met, "", false), "default", specPos, spec.orElse(Some("no spec")).map(_.toString))

  /** this html works well in a diesel fiddle, use toHtmlInPage elsewhere */
  override def toHtml = {
  /*span(arch+"::")+*/first(pos) + eaHtml + " " + toHtmlAttrs(attrs)
  }

  /** as opposed to toHtml, this will produce an html that can be displayed in any page, not just the fiddle */
  def toHtmlInPage = hrefBtnGlobal /*+hrefBtn1*/ + toHtml.replaceAllLiterally("weref", "wefiddle") + "<br>"

  def hrefBtnGlobal =
    s"""<a href="${url2("")}" class="btn btn-xs btn-primary" title="global link">
       |<span class="glyphicon glyphicon glyphicon-th-list"></span></a>""".stripMargin
  def hrefBtnLocal =
    s"""<a href="${url1("")}" class="btn btn-xs btn-info"    title="local link in this topic">
       |<span class="glyphicon glyphicon-list-alt"></span></a>""".stripMargin

  override def toString =
    s""" $entity.$met (${attrs.mkString(", ")})"""

  private def attrsToUrl (attrs: Attrs) = {
    attrs.map{p=>
      // todo only if the expr is constant?
      var v = p.expr.map(_.expr).getOrElse(p.currentStringValue)
      v = Enc.toUrl(v) // escape special chars
      s"""${p.name}=$v"""
    }.mkString("&")
  }

  // local invocation url
  private def url1 (section:String="", resultMode:String="value") = {
    var x = s"""/diesel/wreact/${pos.map(_.wpath).mkString}/react/$entity/$met?${attrsToUrl(attrs)}"""
    if (x.endsWith("&") || x.endsWith("?")) ""
    else if (x contains "?") x = x + "&"
    else x = x + "?"
    if("value" != resultMode) x = x + "resultMode="+resultMode

    if(section != "") {
      x = x + "&dfiddle=" + section
    }
    x
  }

  // reactor invocation url
  private def url2 (section:String="", resultMode:String="value") = {
    var x = if ("diesel.rest" == ea) {
      s"""/diesel/rest""" + attrs.find(_.name == "path").map(_.currentStringValue).mkString
    } else {
      s"""/diesel/react/$entity/$met?${attrsToUrl(attrs)}"""
    }

    if (x.endsWith("&") || x.endsWith("?")) ""
    else if (x contains "?") x = x + "&"
    else x = x + "?"
    if("value" != resultMode) x = x + "resultMode="+resultMode

    if(section != "") {
      x = x + "&dfiddle=" + section
    }
    x
  }

  /** make a href
    *
    * @param section
    * @param resultMode
    * @param mapUrl use to add args to url
    * @return
    */
  def toHref (section:String="", resultMode:String="debug", mapUrl:String=>String=identity) = {
    var u = url1(section, resultMode)
    s"""<a target="_blank" href="${mapUrl(u)}">$entity.$met(${attrs.mkString(", ")})</a>"""
  }

  /** make a href
    *
    * @param text
    * @param section
    * @param resultMode
    * @param mapUrl use to add args to url
    * @return
    */
  def toHrefWith (text:String, section:String="", resultMode:String="debug", mapUrl:String=>String=identity) = {
    var u = url1(section, resultMode)
    s"""<a target="_blank" href="${mapUrl(u)}">$text</a>"""
  }

  override def shouldPrune = this.stype contains PRUNE
  override def shouldIgnore = this.stype contains IGNORE
  override def shouldSkip = this.stype contains SKIP
  override def shouldRollup =
    // rollup simple assignments - leave only the EVals
    entity == "" && met == "" ||
      (this.stype contains ROLLUP)

  /** is it public visilibity */
  def isPublic =
    spec.map(_.arch).exists(_ contains PUBLIC) ||
      spec.map(_.stype).exists(_ contains PUBLIC)

  // todo not used right now - hassle since you have a trust list anyways...
  def isProtected =
    spec.map(_.arch).exists(_ contains PROTECTED) ||
    spec.map(_.stype).exists(_ contains PROTECTED)

  /** use the right parm types **/
  def typecastParms(spec: Option[EMsg]) : EMsg = {
    spec.map { spec =>
      val newParms = attrs.map { p =>
        spec.attrs.find(_.name == p.name).map { s =>
          if (s.ttype != "" && p.ttype == "") {
            if (s.ttype == "Number") {
              if (p.currentStringValue.contains("."))
                p.copy(ttype = s.ttype).withValue(p.currentStringValue.toDouble)
              else
                p.copy(ttype = s.ttype).withValue(p.currentStringValue.toInt)
            } else {
              p.copy(ttype = s.ttype)
            }
          } else {
            p
          }
        }.getOrElse {
          p
        }
      }

      this.copy(attrs = newParms)
    }.getOrElse {
      this
    }
  }
}

object EMsg {
  val PRUNE = "prune"
  val ROLLUP = "rollup"
  val IGNORE = "ignore"
  val SKIP = "skip"

  val PUBLIC = "public"
  val PROTECTED = "protected"

  val WARNING = "warn"

  val REGEX = """([\w.]+)[./](\w+)""".r

  def apply(ea:String) : EMsg = {
    val REGEX(ee, aa) = ea
    EMsg(ee, aa)
  }

}
