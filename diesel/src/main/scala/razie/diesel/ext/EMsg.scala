/**
 *  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.ext

import razie.diesel.dom.RDOM._
import razie.diesel.dom._

import scala.Option.option2Iterable

// a nvp - can be a spec or an event, message, function etc
case class EMsg(entity: String, met: String, attrs: List[RDOM.P]=Nil, arch:String="", ret: List[RDOM.P] = Nil, stype: String = "") extends CanHtml with HasPosition {
  var spec: Option[EMsg] = None
  var pos : Option[EPos] = None
  def withPos(p:Option[EPos]) = {this.pos = p; this}
  def withSpec(p:Option[EMsg]) = {this.spec = p; this}

  def asCtx (implicit ctx:ECtx) : ECtx = new StaticECtx(this.attrs, Some(ctx))

  def toj : Map[String,Any] =
    Map (
      "class" -> "EMsg",
      "arch" -> arch,
      "entity" -> entity,
      "met" -> met,
      "attrs" -> attrs.map { p =>
        Map(
          "name" -> p.name,
          "value" -> p.dflt
        )
      },
      "ret" -> ret.map { p =>
        Map(
          "name" -> p.name,
          "value" -> p.dflt
        )
      },
      "stype" -> stype
    ) ++ {
      pos.map{p=>
        Map ("ref" -> p.toRef,
          "pos" -> p.toJmap
        )
      }.getOrElse(Map.empty)
    }

  // if this was an instance and you know of a spec
  private def first: String = spec.map(_.first).getOrElse(
    kspan("msg:", resolved, spec.flatMap(_.pos)) + span(stype, "info")
  )

  /** if has executor */
  def hasExecutor:Boolean = Executors.all.exists(_.test(this)(ECtx.empty))

  def isResolved:Boolean = if(spec.exists(_.isResolved)) true else (
    if ((stype contains "GET") || (stype contains "POST") || hasExecutor ) true
    else false
  )

  /** color - if has executor */
  private def resolved: String = if(isResolved) "default" else "warning"

  /** extract a match from this message signature */
  def asMatch = EMatch(entity, met, attrs.filter(_.dflt != "").map {p=>
    PM (p.name, p.ttype, p.ref, p.multi, "==", p.dflt)
  })

  /** this html works well in a diesel fiddle, use toHtmlInPage elsewhere */
  override def toHtml = {
  /*span(arch+"::")+*/first + s""" ${ea(entity,met)} """ + toHtmlAttrs(attrs)
  }

  /** as opposed to toHtml, this will produce an html that can be displayed in any page, not just the fiddle */
  def toHtmlInPage = hrefBtn2+hrefBtn1 + toHtml.replaceAllLiterally("weref", "wefiddle") + "<br>"

  private def hrefBtn2 =
    s"""<a href="${url2("")}" class="btn btn-xs btn-primary" title="global link">
       |<span class="glyphicon glyphicon glyphicon-th-list"></span></a>""".stripMargin
  private def hrefBtn1 =
    s"""<a href="${url1("")}" class="btn btn-xs btn-info"    title="local link in this topic">
       |<span class="glyphicon glyphicon-list-alt"></span></a>""".stripMargin

  override def toString =
    s""" $entity.$met (${attrs.mkString(", ")})"""

  private def attrsToUrl (attrs: Attrs) = {
    attrs.map{p=>
      s"""${p.name}=${p.dflt}"""
    }.mkString("&")
  }

  // local invocation url
  private def url1 (section:String="", resultMode:String="value") = {
    var x = s"""/diesel/wreact/${pos.map(_.wpath).mkString}/react/$entity/$met?${attrsToUrl(attrs)}"""
    if (x.endsWith("&") || x.endsWith("?")) ""
    else if (x contains "?") x = x + "&"
    else x = x + "?"
    x = x + "resultMode="+resultMode

    if(section != "") {
      x = x + "&dfiddle=" + section
    }
    x
  }

  // reactor invocation url
  private def url2 (section:String="", resultMode:String="value") = {
    var x = s"""/diesel/fiddle/react/$entity/$met?${attrsToUrl(attrs)}"""
    if (x.endsWith("&") || x.endsWith("?")) ""
    else if (x contains "?") x = x + "&"
    else x = x + "?"
    x = x + "resultMode="+resultMode

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
}
