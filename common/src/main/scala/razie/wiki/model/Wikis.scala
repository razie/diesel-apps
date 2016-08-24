/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model

import com.mongodb.DBObject
import com.mongodb.casbah.Imports._
import com.novus.salat._
import razie.{cdebug, Logging}
import razie.db.{RMany, RazMongo}
import razie.db.RazSalatContext._
import razie.wiki.{Enc, WikiConfig, Services}
import razie.wiki.parser.{WAST, ParserSettings}
import razie.wiki.util.{VErrors, Validation}
import razie.wiki.admin.{Audit}

/** wiki factory and utils */
object Wikis extends Logging with Validation {
  //todo per realm
  /** these categories are persisted in their own tables */
  final val PERSISTED = Array("Item", "Event", "Training", "Note", "Entry", "Form",
    "DslReactor", "DslElement", "DslDomain", "JSON", "DslEntity")

  /** customize table names per category */
  final val TABLE_NAME = "WikiEntry"
  // map all Dsl type entities in the same table
  final val TABLE_NAMES = Map("DslReactor" -> "weDsl", "DslElement" -> "weDsl", "DslDomain" -> "weDsl", "DslEntity" -> "weDslEntity")

  final val RK = WikiConfig.RK
  final val DFLT = RK // todo replace with RK

  def apply (realm:String = RK) = Reactors(realm).wiki
  def rk = Reactors(RK).wiki
  def dflt = Reactors(Reactors.WIKI).wiki

  def fromGrated[T <: AnyRef](o: DBObject)(implicit m: Manifest[T]) = grater[T](ctx, m).asObject(o)

  // TODO refactor convenience
  def find(wid: WID): Option[WikiEntry] = apply(wid.getRealm).find(wid)

  // TODO find by ID is bad, no - how to make it work across wikis ?
  /** @deprecated optimize with realm */
  def findById(id: String) = find(new ObjectId(id))
  /** @deprecated optimize with realm */
  def find(id: ObjectId) =
    Reactors.reactors.foldLeft(None.asInstanceOf[Option[WikiEntry]])((a,b) => a orElse b._2.wiki.find(id))
  /** @deprecated optimize with realm */
  def findById(cat:String, id: String):Option[WikiEntry] = findById(cat, new ObjectId(id))
  /** @deprecated optimize with realm */
  def findById(cat:String, id: ObjectId): Option[WikiEntry] =
    Reactors.reactors.foldLeft(None.asInstanceOf[Option[WikiEntry]])((a,b) => a orElse b._2.wiki.findById(cat, id))

  /** @deprecated use realm */
  def category(cat: String) =
    if(cat.contains(".")) {
      val cs = cat.split("\\.")
      apply(cs(0)).category(cs(1))
    }
    else rk.category(cat)

  def linksFrom(from: UWID) = RMany[WikiLink]("from" -> from.grated)

  def linksTo(to: UWID) = RMany[WikiLink]("to" -> to.grated)

  def childrenOf(parent: UWID) =
    RMany[WikiLink]("to" -> parent.grated, "how" -> "Child").map(_.from)

  def linksFrom(from: UWID, role: String) =
    RMany[WikiLink]("from" -> from.grated, "how" -> role)

//  def linksTo(to: UWID, role: String) =
//    RMany[WikiLink]("to.cat" -> to.cat, "to.id"->to.id, "how" -> role)
//
  // not taking realm into account...
  def linksTo(cat:String, to: UWID, role: String) =
    RMany[WikiLink]("from.cat" -> cat, "to.cat" -> to.cat, "to.id"->to.id, "how" -> role)

  // leave these vvvvvvvvvvvvvvvvvvvvvvvvvv

  def label(wid: WID):String = /*wid.page map (_.label) orElse*/
    apply(wid.getRealm).label(wid)

  def label(wid: UWID):String = /*wid.page map (_.label) orElse*/
    wid.wid.map(x=>label(x)).getOrElse(wid.nameOrId)

  // leave these ^^^^^^^^^^^^^^^^^^^^^^^^^^

    
   //todo refactor in own utils  vvv
    
  final val MD = "md"
  final val TEXT = "text"
  final val JS = "js"
  final val SCALA = "scala"
  final val JSON = "json"
  final val XML = "xml"

  /** helper to deal with the different markups */
  object markups {
    final val list = Seq(MD->"Markdown", TEXT->"Text", JSON->"JSON", XML->"XML", JS->"JavaScript", SCALA->"Scala") // todo per reator type - hackers like stuff

    def contains (s:String) = list.exists(_._1 == s)

    def isDsl (s:String) =
      s == JS || s == XML || s == JSON || s == SCALA
  }



  private def iformatName(name: String, pat: String, pat2: String = "") = name.replaceAll(pat, "_").replaceAll(pat2, "").replaceAll("_+", "_").replaceFirst("_$", "")

  /** format a simple name - try NOT to use this */
  //  def formatName(name: String): String = iformatName(name, """[ &?,;/:{}\[\]]""")

  /** these are the safe url characters. I also included ',which are confusing many sites */
  val SAFECHARS = """[^0-9a-zA-Z\$\-_\.()',]""" // DO NOT TOUCH THIS PATTERN!

  def formatName(name: String): String = iformatName(name, SAFECHARS, "") // DO NOT TOUCH THIS PATTERN!

  /** format a complex name cat:name */
  def formatName(wid: WID): String =
    if ("WikiLink" == wid.cat)
      iformatName(wid.name, """[ /{}\[\]]""")
    else
      formatName(wid.name)

  /** format an even more complex name
    * @param rk force links back to RK main or leave them
    */
  def formatWikiLink(curRealm:String, wid: WID, nicename: String, label: String, role:Option[String], hover: Option[String] = None, rk: Boolean = false) = {
    val name = formatName(wid.name)
    val title = hover.map("title=\"" + _ + "\"") getOrElse ("")

    val r = wid.realm.getOrElse(curRealm)
    // all pages wihtout realm are assumed in current realm

    val bigName = Wikis.apply(r).index.getForLower(name.toLowerCase())
    if (bigName.isDefined || wid.cat.matches("User")) {
      var newwid = Wikis.apply(r).index.getWids(bigName.get).headOption getOrElse wid.copy(name=bigName.get)
//      var newwid = wid.copy(name=bigName.get)
      var u = Services.config.urlmap(newwid.formatted.urlRelative(curRealm))

      if (rk && (u startsWith "/")) u = "http://" + Services.config.rk + u

      (s"""<a href="$u" title="$title">$label</a>""", Some(ILink(newwid, label, role)))
    } else if (rk) {
      val sup = "" //"""<sup><b style="color:red">^</b></sup></a>"""
      (s"""<a href="http://${Services.config.rk}${wid.formatted.urlRelative}" title="$title">$label$sup</a>""" ,
        Some(ILink(wid, label, role)))
    } else {
      // topic not found in index - hide it from google
      val prefix = if(wid.realm.isDefined && wid.getRealm != curRealm) s"/we/${wid.getRealm}" else "/wikie"
      val plusplus = if(Wikis.PERSISTED.contains(wid.cat)) "" else """<sup><b style="color:red">++</b></sup>"""
      (s"""<a href="$prefix/show/${wid.wpath}" title="%s">$label$plusplus</a>""".format
        (hover.getOrElse("Missing page")),
        Some(ILink(wid, label, role)))
    }
  }

  def shouldFlag(name: String, label: String, content: String): Option[String] = {
    val a = Array(name, label, content)

    if (a.exists(_.matches("(?i)^.*<(" + ParserSettings.hnok + ")([^>]*)>"))) Some("WIKI_FORBIDDEN_HTML")
    else if (hasBadWords(content, adultWords)) Some("WIKI_HAS_ADULT")
    else None
  }

  private def include(wid:WID, c2: String)(implicit errCollector: VErrors): Option[String] = {
    var done = false
    val res = try {
      val INCLUDE = """(?<!`)\[\[include(WithSection)?:([^\]]*)\]\]""".r
      val res1 = INCLUDE.replaceAllIn(c2, { m =>
        val content = for (
          iwid <- WID.fromPath(m.group(2)).map(w=> if(w.realm.isDefined) w else w.r(wid.getRealm)) orErr ("bad format for page");
          c <- (if(m.group(1) == null) iwid.content else iwid.findSection.map(_.original)) orErr s"content for ${iwid.wpath} not found"
        ) yield c

        done = true
        //regexp uses $ as a substitution
        content.map(_.replaceAll("\\$", "\\\\\\$")).getOrElse("`[ERR Can't include $1 " + errCollector.mkString + "]`")
      })

      val TEMPLATE = """(?<!`)\[\[template:([^\]]*)\]\]""".r
      TEMPLATE.replaceAllIn(res1, { m =>
        done = true
        //todo this is parse-ahead, maybe i can make it lazy?
        val parms = WikiForm.parseFormData(c2)
        val content = template (m.group(1), Map()++parms)
        //regexp uses $ as a substitution
        content.replaceAll("\\$", "\\\\\\$")
      })
    } catch {
      case s: Throwable => log("Error: ", s); "`[ERR Can't process an include]`"
    }
    if (done) Some(res) else None
  }

  def preprocessIncludes(wid: WID, markup: String, content: String) = markup match {
    case MD =>
      implicit val errCollector = new VErrors()

      var c2 = content

      // TODO stupid - 3 levels of include...
      include(wid, c2).map { c2 = _ }.flatMap { x =>
        include(wid, c2).map { c2 = _ }.flatMap { x =>
          include(wid, c2).map { c2 = _ }
        }
      }

      c2

    case _ => content
  }

  // TODO better escaping of all url chars in wiki name
  def preprocess(wid: WID, markup: String, content: String, page:Option[WikiEntry]) = markup match {
    case MD =>
      val t1 = System.currentTimeMillis
      implicit val errCollector = new VErrors()

      var c2 = content

      if (c2 contains "[[./")
        c2 = content.replaceAll("""\[\[\./""", """[[%s/""".format(wid.realm.map(_ + ".").mkString + wid.cat + ":" + wid.name)) // child topics
      if (c2 contains "[[../")
        c2 = c2.replaceAll("""\[\[\../""", """[[%s""".format(wid.parentWid.map(wp => wp.realm.map(_ + ".").mkString + wp.cat + ":" + wp.name + "/").getOrElse(""))) // siblings topics

      // TODO stupid - 3 levels of include...
      include(wid, c2).map { c2 = _ }.flatMap { x =>
        include(wid, c2).map { c2 = _ }.flatMap { x =>
          include(wid, c2).map { c2 = _ }
        }
      }

      // pre-mods
      page.orElse(wid.page).map { x => c2 = razie.wiki.mods.WikiMods.modPreParsing(x, Some(c2)).getOrElse(c2) }

      val res = Reactors(wid.getRealm).wiki.mkParser apply c2
      val t2 = System.currentTimeMillis
      cdebug << s"wikis.preprocessed ${t2 - t1} millis for ${wid.name}"
      res

    case TEXT => WAST.SState(content.replaceAll("""\[\[([^]]*)\]\]""", """[[\(1\)]]"""))
    case JSON | XML | JS | SCALA => WAST.SState(content)

    case _ => WAST.SState("UNKNOWN MARKUP " + markup + " - " + content)
  }

  /** html for later */
  def propLater (id:String, url:String) = s"""<script async>$$("#$id").load("$url");</script>"""

  /** partial formatting function
    *
    * @param wid - the wid being formatted
    * @param markup - markup language being formatted
    * @param icontent - the content being formatted or "" if there is a WikiEntry being formatted
    * @param we - optional page for context for formatting
    * @return
    */
  private def format1(wid: WID, markup: String, icontent: String, we: Option[WikiEntry], user:Option[WikiUser]) = {
    val res = try {
      var content =
        (if(icontent == null || icontent.isEmpty) {
          if (wid.section.isDefined)
            preprocess(wid, markup, noBadWords(wid.content.mkString), we)
          else
            // use preprocessed cache
            we.map(_.preprocessed).getOrElse(preprocess(wid, markup, noBadWords(icontent), we))
        }
        else
          preprocess(wid, markup, noBadWords(icontent), we)
        ).fold(WAST.context(we, user)).s

      // TODO index nobadwords when saving/loading page, in the WikiIndex
      // TODO have a pre-processed and formatted page index I can use - for non-scripted pages, refreshed on save
      // run scripts
      val S_PAT = """`\{\{(call):([^#}]*)#([^}]*)\}\}`""".r

      content = S_PAT replaceSomeIn (content, { m =>
        try {
          // find the page with the scripts and call them
          val pageWithScripts = WID.fromPath(m group 2).flatMap(x => Wikis(x.getRealm).find(x)).orElse(we)
          pageWithScripts.flatMap(_.scripts.find(_.name == (m group 3))).filter(_.checkSignature(user)).map(s => runScript(s.content, "js", we, user))
        } catch { case _: Throwable => Some("!?!") }
      })

      // TODO this is experimental
//      val E_PAT = """`\{\{(e):([^}]*)\}\}`""".r
//
//      content = E_PAT replaceSomeIn (content, { m =>
//        try {
//          find the page with the scripts and call them
//          if((m group 2) startsWith "api.wix") Some(runScript(m group 2, we))
//          else None
//        } catch { case _: Throwable => Some("!?!") }
//      })

      // cannot have these expanded in the  AST parser because then i recurse forever when resolving XPATHs...
      val XP_PAT = """`\{\{\{(xp[l]*):([^}]*)\}\}\}`""".r

      content = XP_PAT replaceSomeIn (content, { m =>
        try {
          we.map(x => runXp(m group 1, x, m group 2))
        } catch { case _: Throwable => Some("!?!") }
      })

      // for forms
      we.map { x => content = new WForm(x).formatFields(content) }

      // pre-mods
      we.map { x => content = razie.wiki.mods.WikiMods.modPreHtml(x, Some(content)).getOrElse(content) }

      markup match {
        case MD => {

          object DTimer {
            def apply[A](desc:String)(f: => A): A = {
              val t1 = System.currentTimeMillis
              val res:A = f
              val t2 = System.currentTimeMillis
              cdebug << s"$desc took ${t2 - t1} millis"
              res
            }
          }

          val res = DTimer ("wikis.mdhtml for "+wid.name) {
              val ast = DTimer ("wikis.mdast for "+wid.name) {
                val parser = org.commonmark.parser.Parser.builder().build();
                parser.parse(content);
              }

            val renderer = org.commonmark.html.HtmlRenderer.builder().build();
            renderer.render(ast);  // "<p>This is <em>Sparta</em></p>\n"
          }

          res
        }
        case TEXT => content
        case JSON | XML | JS | SCALA => content
        case _ => "UNKNOWN MARKUP " + markup + " - " + content
      }
    } catch {
      case e : Throwable => {
        Audit.logdbWithLink("ERR_FORMATTING", wid.ahref, "[[ERROR FORMATTING]]: " + e.toString)
        log("[[ERROR FORMATTING]]: " + e.toString + "\n"+e.getStackTraceString)
        if(Services.config.isLocalhost) throw e
        "[[ERROR FORMATTING]] - sorry, dumb program here! The content is not lost: try editing this topic... also, please report this topic with the error and we'll fix it for you!"
      }
    }
    res
  }

  def irunXp(what: String, w: WikiEntry, path: String) = {
    var root = new razie.Snakk.Wrapper(new WikiWrapper(w.wid), WikiXpSolver)
    var xpath = path // TODO why am I doing this?

    val ROOT_ALL = """root\(\*\)/(.*)""".r
    val ROOT = """root\(([^:]*):([^:)/]*)\)/(.*)""".r //\[[@]*(\w+)[ \t]*([=!~]+)[ \t]*[']*([^']*)[']*\]""".r

    path match {
      case ROOT_ALL(rest) => {
        root = new razie.Snakk.Wrapper(new WikiWrapper(WID("Admin", "*").r(w.realm)), WikiXpSolver)
        xpath = rest //path.replace("root(*)/", "")
      }
      case ROOT(cat, name, rest) => {
        root = new razie.Snakk.Wrapper(new WikiWrapper(WID(cat, name).r(w.realm)), WikiXpSolver)
        xpath = rest
      }
    }

    val res: List[_] =
      if (razie.GPath(xpath).isAttr) (root xpla xpath).filter(_.length > 0) // sometimes attributes come as zero value?
      else {
        (root xpl xpath).collect {
          case ww: WikiWrapper => formatWikiLink(w.realm, ww.wid, ww.wid.name, ww.page.map(_.label).getOrElse(ww.wid.name), None)._1
        }
      }

    res
  }

  def runXp(what: String, w: WikiEntry, path: String) = {
    val res = irunXp(what, w, path)

    what match {
      case "xp" => res.headOption.getOrElse("?").toString
      case "xpl" => "<ul>" + res.take(100).map { x: Any => "<li>" + x.toString + "</li>" }.mkString + (if(res.size>100)"<li>...</li>" else "") + "</ul>"
//      case "xmap" => res.take(100).map { x: Any => "<li>" + x.toString + "</li>" }.mkString
    }
    //        else "TOO MANY to list"), None))
  }

  // scaled down formatting of jsut some content
  def sformat(content: String, markup:String="md", user:Option[WikiUser]) =
    format (WID("1","2"), markup, content, None, user)

  /** main formatting function
    *
    * @param wid - the wid being formatted
    * @param markup - markup language being formatted
    * @param icontent - the content being formatted or "" if there is a WikiEntry being formatted
    * @param we - optional page for context for formatting
    * @return
    */
  def formatJson(wid: WID, markup: String, icontent: String, we: Option[WikiEntry] = None) = {
    val content =
      if(icontent == null || icontent.isEmpty) wid.content.mkString
      else icontent

    content
  }

  /** main formatting function
   *
   * @param wid - the wid being formatted
   * @param markup - markup language being formatted
   * @param icontent - the content being formatted or "" if there is a WikiEntry being formatted
   * @param we - optional page for context for formatting
   * @return
   */
  def format(wid: WID, markup: String, icontent: String, we: Option[WikiEntry], user:Option[WikiUser]) = {
    if (JSON == wid.cat || JSON == markup || XML == wid.cat || XML == markup)
      formatJson(wid, markup, icontent, we)
    else {
      var res = format1(wid, markup, icontent, we, user)

      // mark the external links
      val sup = "" //"""<sup>&nbsp;<b style="color:darkred">^</b></sup>""")
      val A_PAT = """(<a +href="http://)([^>]*)>([^<]*)(</a>)""".r
      res = A_PAT replaceSomeIn (res, { m =>
        if (Option(m group 2) exists (s=> !s.startsWith(Services.config.hostport)  &&
          !Services.isSiteTrusted(s))
          )
          Some("""$1$2 title="External site"><i>$3</i>"""+sup+"$4")
        else None
      })

      // replace all divs - limitation of the markdown parser
      //      res = res.replaceAll("\\{\\{div ([^}]*)\\}\\}", """<div $1>""")
      val DPAT1 = "\\{\\{div ([^}]*)\\}\\}".r
      res = DPAT1 replaceSomeIn (res, { m =>
        Some("<div "+Enc.unescapeHtml(m group 1)+">")
      })

      res = res.replaceAll("\\{\\{/div *\\}\\}", "</div>")

      //    // modify external sites mapped to external URLs
      //    // TODO optimize - either this logic or a parent-based approach
      //    for (site <- Wikis.urlmap)
      //      res = res.replaceAll ("""<a +href="%s""".format(site._1), """<a href="%s""".format(site._2))

      // get some samples of what people get stuck on...
      if(res contains "CANNOT PARSE")
        Audit.logdbWithLink(
          "CANNOT_PARSE",
          wid.urlRelative,
          s"""${wid.wpath} ver ${we.map(_.ver)}""")

      res
    }
  }

  def divLater(x:String) = {
    val y = x.replaceAll("\\{\\{div.later ([^ ]*) ([^}]*)\\}\\}",
      """
        | <div id=$1>div.later</div>
        | <script type="text/javascript">
        |   \$("#$1").attr("src","$2");
        | </script>
        | """.stripMargin)
//    """
//      | <div id=$1>div.later</div>
//      | <script type="text/javascript">
//      |   \$(document).ready(function(){
//      |     \$("#$1").attr("src","$2");
//      | })
//      | </script>
//      | """.stripMargin
    y
  }

  // todo protect this from tresspassers
  def runScript(s: String, lang:String, page: Option[WikiEntry], au:Option[WikiUser]) = {
    // page preprocessed for, au or default to thread statics - the least reliable
    val up = page.flatMap(_.ipreprocessed.flatMap(_._2)) orElse au //orElse razie.NoStaticS.get[WikiUser]
    //todo use au not up
    val q = razie.NoStaticS.get[QueryParms]
    Services.runScript(s, lang, page, up, q.map(_.q.map(t => (t._1, t._2.mkString))).getOrElse(Map()))
  }

  /** format content from a template, given some parms
    *
    * - this is used only when creating new pages from spec
    *
    * DO NOT mess with this - one side effect is only replacing the ${} it understands...
    *
    * CANNOT should reconcile with templateFromContent
    */
  def template(wpath: String, parms:Map[String,String]) = {
    (for (
      wid <- WID.fromPath(wpath).map(x=>if(x.realm.isDefined) x else x.r("wiki")); // templates are in wiki or rk
      c <- wid.content
    ) yield {
        val s1 = parms.foldLeft(c)((a,b)=>a.replaceAll("\\{\\{\\$\\$"+b._1+"\\}\\}", b._2))
        s1.replaceAll("\\{\\{`", "{{")//.replaceAll("\\{\\{`", "{{").replaceAll("\\{\\{`/section", "{{/section")
      }) getOrElse (
      "No content template for: " + wpath + "\n\nAttributes:\n\n" + parms.map{t=>s"* ${t._1} = ${t._2}\n"}.mkString
      )
  }

  /** format content from a template, given some parms
  *
  * @param parms will resolve expressions from the template into Strings. you can use a Map.
  *              parms("*") should return some details for debugging
  */
  def templateFromContent(content: String, parms:String=>String) = {
    val PAT = """\\$\\{([^\\}]*)\\}""".r
    val s1 = PAT.replaceAllIn(content, {m =>
      parms(m.group(1))
    })
  }

  def noBadWords(s: String) = badWords.foldLeft(s)((x, y) => x.replaceAll("""\b%s\b""".format(y), "BLIP"))

  def hasBadWords(s: String, what: Array[String] = badWords): Boolean = s.toLowerCase.split("""\w""").exists(what.contains(_))

  def flag(we: WikiEntry) { flag(we.wid) }

  def flag(wid: WID, reason: String = "") {
    Audit.logdb("WIKI_FLAGGED", reason, wid.toString)
  }

  final val badWords = "boohoo,hell".split(",")
  final val adultWords = "damn,heck".split(",")

  //todo who uses this
  def updateUserName(uold: String, unew: String) = {
    // TODO 1 optimize with find()
    // TODO 2 rename references
    val we = RazMongo("WikiEntry")
    for (u <- we.findAll() if "User" == u.get("category") && uold == u.get("name")) {
        u.put("name", unew)
        we.save(u)
    }
    val weo = RazMongo("WikiEntryOld")
    for (u <- weo.findAll() if "User" == u.get("category") && uold == u.get("name")) {
      u.put("name", unew)
      weo.save(u)
    }
  }
  
  def w(we: UWID):String = we.wid.map(wid=>w(wid)).getOrElse("ERR_NO_URL_FOR_"+we.toString)
  def w(we: WID, shouldCount: Boolean = true):String =
    Services.config.urlmap(we.urlRelative + (if (!shouldCount) "?count=0" else ""))

  /** make a relative href for the given tag. give more tags with 1/2/3 */
  def hrefTag(wid:WID, t:String,label:String) = {
    if(Array("Blog","Forum") contains wid.cat) {
      s"""<b><a href="${w(wid)}/tag/$t">$label</a></b>"""
    } else {
      if(wid.parentWid.isDefined) {
        s"""<b><a href="${w(wid.parentWid.get)}/tag/$t">$label</a></b>"""
      } else {
        s"""<b><a href="/wiki/tag/$t">$label</a></b>"""
      }
    }
  }

}

object WDOM {

  def apply(realm:String, cat:String) = new {
    def linksTo(cat:String, to: UWID, role: String) = {
      val c = Wikis(realm).category(cat)
//      if(c.flatMap(_.contentTags.get("persistence")).exists(_ == "custom")) {
//        c.get.contentTags("persistence.inventory")
//      } else
      RMany[WikiLink]("from.cat" -> cat, "to" -> to.grated, "how" -> role)
    }
  }

}

