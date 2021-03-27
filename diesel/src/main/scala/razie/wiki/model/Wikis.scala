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
import controllers.{VErrors, Validation}
import play.api.Play.current
import play.api.cache._
import razie.audit.Audit
import razie.db.RazSalatContext._
import razie.db.{RMany, RazMongo}
import razie.diesel.dom.WikiDomain
import razie.hosting.WikiReactors
import razie.tconf.Visibility.PUBLIC
import razie.tconf.parser.{BaseAstNode, LeafAstNode, SpecParserSettings, StrAstNode}
import razie.wiki.model.features.{WForm, WikiForm}
import razie.wiki.parser.WAST
import razie.wiki.util.QueryParms
import razie.wiki.{Enc, Services, WikiConfig}
import razie.{Logging, clog, ctrace}
import scala.collection.mutable.ListBuffer

object WikiCache {

  def set[T](id:String, w:T, i:Int) = {
    clog << "WIKI_CACHE_SET - "+id
    Cache.set(id, w, 300) // 10 miuntes
  }

  def getEntry(id:String) : Option[WikiEntry] = {
    Cache.getAs[WikiEntry](id).map{x=>
      clog << "WIKI_CACHED FULL - "+id
      x
    }
  }

  def getDb(id:String) : Option[DBObject] = {
    Cache.getAs[DBObject](id).map{x=>
      clog << "WIKI_CACHED DB - "+id
      x
    }
  }

  def getString(id:String) : Option[String] = {
    Cache.getAs[String](id).map{x=>
      clog << "WIKI_CACHED FRM - "+id
      x
    }
  }

  def remove(id:String) = {
    clog << "WIKI_CACHE_CLEAR - "+id
    Cache.remove(id)
  }
}

/** wiki factory and utils */
object Wikis extends Logging with Validation {

  /** create the data section */
  def mkFormData(spec: WikiEntry, defaults: Map[String, String] = Map.empty) = {
    // build the defaults - cross check with formSpec
    var defaultStr = ""
    defaults.filter(x=> spec.form.fields.contains(x._1)).map { t =>
      val (k, v) = t
      defaultStr = defaultStr + s""", "$k":"$v" """
    }

    val content = s"""
{{.section:formData}}
{"formState":"created" $defaultStr }
{{/section}}
"""

    content
  }

  def isEvent(cat: String) = "Race" == cat || "Event" == cat || "Training" == cat

  //todo configure per realm
  /** these categories are persisted in their own tables */
  final val PERSISTED = Array("Item", "Event", "Training", "Note", "Entry", "Form", "JSON")
//    "DslReactor", "DslElement", "DslDomain", "JSON", "DslEntity")

  /** customize table names per category */
  final val TABLE_NAME = "WikiEntry"
  // map all Dsl type entities in the same table
  final val TABLE_NAMES = Map.empty[String,String]
  //("DslReactor" -> "weDsl", "DslElement" -> "weDsl", "DslDomain" -> "weDsl", "DslEntity" -> "weDslEntity")

  final val RK = WikiConfig.RK
  final val DFLT = RK // todo replace with RK

  def apply(realm: String = RK) = WikiReactors(realm).wiki

  def rk = WikiReactors(RK).wiki

  def dflt = WikiReactors(WikiReactors.WIKI).wiki

  def fromGrated[T <: AnyRef](o: DBObject)(implicit m: Manifest[T]) = grater[T](ctx, m).asObject(o)

  /** safe to call before reactors are initialized */
  def findSimple (wid:WID) = {
    RazMongo(Wikis.TABLE_NAME).findOne(Map("category" -> wid.cat, "name" -> wid.name)) map (grater[WikiEntry].asObject(_))
  }

  // TODO refactor convenience
  def find(wid: WID): Option[WikiEntry] =
    apply(wid.getRealm).find(wid)

  // TODO find by ID is bad, no - how to make it work across wikis ?
  /** @deprecated optimize with realm */
  def findById(id: String) = find(new ObjectId(id))

  /** @deprecated optimize with realm */
  def find(id: ObjectId) =
    WikiReactors.reactors.foldLeft(None.asInstanceOf[Option[WikiEntry]])((a, b) => a orElse b._2.wiki.find(id))

  /** @deprecated optimize with realm */
  def findById(cat: String, id: ObjectId): Option[WikiEntry] =
    WikiReactors.reactors.foldLeft(None.asInstanceOf[Option[WikiEntry]])((a, b) => a orElse b._2.wiki.findById(cat, id))

  def linksFrom(to: UWID) = RMany[WikiLink]("from.cat" -> to.cat, "from.id" -> to.id)

  def linksTo(to: UWID) = RMany[WikiLink]("to.cat" -> to.cat, "to.id" -> to.id)

  def childrenOf(parent: UWID) =
    RMany[WikiLink]("to.id" -> parent.id, "how" -> "Child").map(_.from)

  def linksFrom(from: UWID, role: String) =
    RMany[WikiLink]("from.id" -> from.id, "how" -> role)

  // not taking realm into account...
  def linksTo(cat: String, to: UWID, role: String) =
  RMany[WikiLink]("from.cat" -> cat, "to.cat" -> to.cat, "to.id" -> to.id, "how" -> role)

  // leave these vvvvvvvvvvvvvvvvvvvvvvvvvv

  def label(wid: WID): String = /*wid.page map (_.label) orElse*/
    apply(wid.getRealm).label(wid)

  def label(wid: UWID): String = /*wid.page map (_.label) orElse*/
    wid.wid.map(x => label(x)).getOrElse(wid.nameOrId)

  // leave these ^^^^^^^^^^^^^^^^^^^^^^^^^^


  //todo refactor in own utils  vvv

  final val MD = "md"
  final val TEXT = "text"
  final val JS = "js"
  final val SCALA = "scala"
  final val JSON = "json"
  final val XML = "xml"
  final val HTML = "html"

  /** helper to deal with the different markups */
  object markups {

    final val list = Seq(
      MD -> "Markdown",
      TEXT -> "Text",
      JSON -> "JSON",
      XML -> "XML",
      JS -> "JavaScript",
      SCALA -> "Scala",
      HTML -> "Raw html"
    ) // todo per reator type - hackers like stuff

    def contains(s: String) = list.exists(_._1 == s)

    def isDsl(s: String) =
      s == JS || s == XML || s == JSON || s == SCALA
  }

  def formFor(we: WikiEntry) = {
    we.attr("wiki.form") orElse WikiDomain(we.realm).prop(we.category, "inst.form")
  }

  def templateFor(we: WikiEntry) = {
    we.attr("wiki.template") orElse WikiDomain(we.realm).prop(we.category, "inst.template")
  }


  private def iformatName(name: String, pat: String, pat2: String = "") =
    name.replaceAll(pat, "_").replaceAll(pat2, "").replaceAll("_+", "_").replaceFirst("_$", "")

  /** format a simple name - try NOT to use this */

  /** these are the safe url characters. I also included ',which are confusing many sites */
  val SAFECHARS =
    """[^0-9a-zA-Z\$\-_()',]""" // DO NOT TOUCH THIS PATTERN!

  def formatName(name: String): String = iformatName(name, SAFECHARS, "") // DO NOT TOUCH THIS PATTERN!

  /** format a complex name cat:name */
  def formatName(wid: WID): String =
    if ("WikiLink" == wid.cat)
      iformatName(wid.name, """[ /{}\[\]]""")
    else
      formatName(wid.name)

  /** format an even more complex name
    *
    * @param rk force links back to RK main or leave them
    */
  def formatWikiLink(curRealm: String, wid: WID, nicename: String, label: String, role: Option[String], hover: Option[String] = None, rk: Boolean = false, max: Int = -1) = {
    val name = formatName(wid.name)
    val title = hover.map("title=\"" + _ + "\"") getOrElse ("")

    def trim(s: String) = {
      if (max < 0) s
      else {
        if (s.length > max) s.substring(0, max - 3) + "..."
        else s
      }
    }

    val tlabel = trim(label)

    val r = wid.realm.getOrElse(curRealm)
    // all pages wihtout realm are assumed in current realm

    val bigName = Wikis.apply(r).index.getForLower(name.toLowerCase())
    if (bigName.isDefined || wid.cat.matches("User")) {
      var newwid = Wikis.apply(r).index.getWids(bigName.get).headOption.map(_.copy(section = wid.section)) getOrElse wid.copy(name = bigName.get)
      var u = newwid.formatted.urlRelative(curRealm)

      if (rk && (u startsWith "/")) u = "http://" + Services.config.home + u

      (s"""<a href="$u" title="$title">$tlabel</a>""", Some(ILink(newwid, label, role)))
    } else if (rk) {
      val sup = "" //"""<sup><b style="color:red">^</b></sup></a>"""
      (
        s"""<a href="http://${Services.config.home}${wid.formatted.urlRelative}" title="$title">$tlabel$sup</a>""",
        Some(ILink(wid, label, role)))
    } else {
      // topic not found in index - hide it from google
      //      val prefix = if (wid.realm.isDefined && wid.getRealm != curRealm) s"/we/${wid.getRealm}" else "/wikie"
      val prefix = "/wikie"
      val plusplus = if (Wikis.PERSISTED.contains(wid.cat)) "" else """<sup><b style="color:red">++</b></sup>"""
      (
        s"""<a href="$prefix/show/${wid.wpath}" title="%s">$tlabel$plusplus</a>""".format
        (hover.getOrElse("Missing page")),
        Some(ILink(wid, label, role)))
    }
  }

  def shouldFlag(name: String, label: String, content: String): Option[String] = {
    val a = Array(name, label, content)

    if (a.exists(_.matches("(?i)^.*<(" + SpecParserSettings.hnok + ")([^>]*)>"))) Some("WIKI_FORBIDDEN_HTML")
    else if (hasBadWords(content, adultWords)) Some("WIKI_HAS_ADULT")
    else None
  }

  private def include(wid: WID, c2: String, we: Option[WikiEntry] = None, firstTime: Boolean = false)(implicit errCollector: VErrors): Option[String] = {
    // todo this is not cached as the underlying page may change - need to pick up changes
    var done = false
    val collecting = we.exists(_.depys.isEmpty) // should collect depys

    val res = try {
      val INCLUDE = """(?<!`)\[\[include(WithSection)?:([^\]]*)\]\]""".r
      var res1 = INCLUDE.replaceAllIn(c2, { m =>
        val content = for (
          iwid <- WID.fromPath(m.group(2)).map(w => if (w.realm.isDefined) w else w.r(wid.getRealm)) orErr ("bad format for page");
          c <- (if (m.group(1) == null) iwid.content else iwid.findSection.map(_.original)) orErr s"content for ${iwid.wpath} not found"
        ) yield {
          if (collecting && we.isDefined)
            we.get.depys = iwid.uwid.toList ::: we.get.depys
          c
        }

        done = true

        // IF YOUR content changes - review this escape here
        //regexp uses $ as a substitution
        val xx = content
          .map(
            _.replaceAllLiterally("\\", "\\\\")
             .replaceAll("\\$", "\\\\\\$")
          )
//          .map(_.replaceAllLiterally("$", "\\$"))
//          .map(_.replaceAll("\\\\", "\\\\\\\\"))
          .getOrElse("`[ERR Can't include $1 " + errCollector.mkString + "]`")

        xx
      })

      if (!res1.contains("{{.wiki.noTemplate")) {
        var hadTemplate = false
        val TEMPLATE = """(?<!`)\{\{\.?wiki.template[: ]*([^\}]*)\}\}""".r
        res1 = TEMPLATE.replaceAllIn(res1, { m =>
          done = true
          hadTemplate = true
          //todo this is parse-ahead, maybe i can make it lazy?
          val parms = WikiForm.parseFormData(c2)
          val content = template(m.group(1), Map() ++ parms)
          // IF YOUR content changes - review this escape here
          //regexp uses $ as a substitution
          content
            .replaceAllLiterally("\\", "\\\\")
            .replaceAll("\\$", "\\\\\\$")
        })

        // check cat for preloaded cats that will trigger stackoverflow
        // also, while domain is loading itself, i'm not processing instance templates
        if (firstTime && !hadTemplate && wid.cat != "Category" && wid.cat != "Reactor" && !WikiDomain(wid.getRealm).isLoading)
          WikiDomain(wid.getRealm).prop(wid.cat, "inst.template").map { t =>
            done = true
            val parms = WikiForm.parseFormData(c2)
            val content = template(t, Map() ++ parms)

            res1 = content + "\n\n" + res1
          }
      }

      res1
    } catch {
      case s: Throwable => log("Error: ", s); "`[ERR Can't process an include]`"
    }
    if (done) Some(res) else None
  }

  def preprocessIncludes(wid: WID, markup: String, content: String, page: Option[WikiEntry] = None) = markup match {
    case MD =>
      implicit val errCollector = new VErrors()

      var c2 = content

      // TODO stupid - 3 levels of include...
      include(wid, c2, page, true).map {
        c2 = _
      }.flatMap { x =>
        include(wid, c2, page, false).map {
          c2 = _
        }.flatMap { x =>
          include(wid, c2, page, false).map {
            c2 = _
          }
        }
      }

      c2

    case _ => content
  }

  // TODO better escaping of all url chars in wiki name
  /** pre-process this wiki: do  AST, includes etc */
  def preprocess(wid: WID, markup: String, content: String, page: Option[WikiEntry]) : (BaseAstNode, String) = {
    implicit val errCollector = new VErrors()
    
    def includes (c:String) = {
      var c2 = c

      if (c2 contains "[[./")
        c2 = c.replaceAll("""\[\[\./""", """[[%s/""".format(wid.realm.map(_ + ".").mkString + wid.cat + ":" + wid.name)) // child topics
      if (c2 contains "[[../")
        c2 = c2.replaceAll("""\[\[\../""", """[[%s""".format(wid.parentWid.map(wp => wp.realm.map(_ + ".").mkString + wp.cat + ":" + wp.name + "/").getOrElse(""))) // siblings topics

      // TODO stupid - 3 levels of include...
      include(wid, c2, page, true).map { x =>
        page.map(_.cacheable = false) // simple dirty if includes, no depy to manage
        c2 = x
      }.flatMap { x =>
        include(wid, c2, page, false).map {
          c2 = _
        }.flatMap { x =>
          include(wid, c2, page, false).map {
            c2 = _
          }
        }
      }

      c2
    }

    try {
      markup match {
        case MD =>
          val t1 = System.currentTimeMillis

          var c2 = includes(content)

          // pre-mods
          page.orElse(wid.page).map { x =>
            // WikiMods will dirty the we.cacheable if needed
            c2 = razie.wiki.mods.WikiMods.modPreParsing(x, Some(c2)).getOrElse(c2)
          }

          val res = WikiReactors(wid.getRealm).wiki.mkParser apply c2
          val t2 = System.currentTimeMillis
          ctrace << s"wikis.preprocessed ${t2 - t1} millis for ${wid.name}"
          (res, c2)

        case TEXT => {
          val c2 = content.replaceAll("""\[\[([^]]*)\]\]""", """[[\(1\)]]""")
          (StrAstNode(c2), c2)
        }

        case JSON | XML | JS | SCALA => {
          (StrAstNode(content), content)
        }

        case HTML => {
          // trick: parse it like we normally would, for properties and includes, but then discard
          val x = preprocess(wid, MD, content, page)
          (LeafAstNode(x._2, x._1), x._2)
        }

        case _ => (StrAstNode("UNKNOWN_MARKUP " + markup + " - " + content), content)
      }
    } catch {
      case t: Throwable =>
        razie.Log.error("EXCEPTION_PARSING " + markup + " - " + wid.wpath, t)
        razie.audit.Audit.logdb("EXCEPTION_PARSING", markup + " - " + wid.wpath + " " + t.getLocalizedMessage())
        (StrAstNode("EXCEPTION_PARSING " + markup + " - " + t.getLocalizedMessage() + " - " + content), content)
    }
  }


  /** html for later */
  def propLater (id:String, url:String) =
    s"""<script async>require(['jquery'],function($$){$$("#$id").load("$url");});</script>"""

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
            preprocess(wid, markup, noBadWords(wid.content.mkString), we)._1
          else
            // use preprocessed cache
            we.flatMap(_.ipreprocessed.map(_._1)).orElse(
              we.map(_.preprocess(user))
            ).getOrElse(
              preprocess(wid, markup, noBadWords(icontent), we)._1
            )
        }
        else
          preprocess(wid, markup, noBadWords(icontent), we)._1
        ).fold(WAST.context(we, user)).s

      // apply md templates first
      content = Wikis(wid.getRealm).applyTemplates(wid, content, "md")

      // TODO index nobadwords when saving/loading page, in the WikiIndex
      // TODO have a pre-processed and formatted page index I can use - for non-scripted pages, refreshed on save
      // run scripts
      val S_PAT = """`\{\{(call):([^#}]*)#([^}]*)\}\}`""".r

      try {
        // to evaluate scripts wihtout a page, we need this trick:
        val tempPage = we orElse None //Some(new WikiEntry("Temp", "fiddle", "fiddle", "md", content, new ObjectId(), Seq("temp"), ""))

        // warn against duplicated included scripts
        val duplicates = new ListBuffer[String]()

      content = S_PAT replaceSomeIn (content, { m =>
        we.map(_.cacheable = false)
        try {
          // find the page with signed scripts and call them
          // inline scripts are exanded into the html page
          val scriptName = m group 3
          val scriptPath = m group 2
          val pageWithScripts = WID.fromPath(scriptPath).flatMap(x => Wikis(x.getRealm).find(x)).orElse(tempPage)
          val y=pageWithScripts.flatMap(_.scripts.find(_.name == scriptName)).filter(_.checkSignature(user)).map{s=>
            val warn = if(duplicates contains s.name) {
              s"`WARNING: script named '${s.name}' duplicated - check your includes`\n\n"
            } else ""

            duplicates.append(s.name)

            if("inline" == s.stype) {
              val wix = Wikis(wid.getRealm).mkWixJson(we, user, Map.empty, "")
              warn + s"""<!-- WikiScript: ${s} -->
                |<script>
                |withJquery(function(){
                |${wix}\n
                |${s.content}
                |;});
                |</script>
              """.stripMargin
            } else
              runScript(s.content, "js", we, user)
        }
          // dolar sign (jquery) in embedded JS needs to be escaped ... don't remember why
          y
            .map(_.replaceAll("\\$", "\\\\\\$"))
            // also, any escaped double quote needs re-escaped... likely same reason as dolar sign
            // wix.toJson can escape realm props including "" and they get lost somehow if I don't do this
            .map(_.replaceAll("\\\"", "\\\\\\\""))
        } catch {
          case t: Throwable => {
            log("exception in script", t)
            Some("`!?!`")
          }
        }
      })
      } catch {
        // sometimes the pattern itself blows
        case t: Throwable => log("exception in script", t);
      }

      // cannot have these expanded in the  AST parser because then i recurse forever when resolving XPATHs...
      val XP_PAT = """`\{\{\{(xp[l]*):([^}]*)\}\}\}`""".r

      content = XP_PAT replaceSomeIn (content, { m =>
        we.map(_.cacheable = false)
        try {
          we.map(x => runXp(m group 1, x, m group 2))
        } catch { case _: Throwable => Some("!?!") }
      })

      // for forms
      we.map { x => content = new WForm(x).formatFields(content) }

      // pre-mods
      we.map {x =>
        // we don't mark cacheable false - the WikiMods does that
        content = razie.wiki.mods.WikiMods.modPreHtml(x, Some(content)).getOrElse(content)
      }

      //todo plugins register and define formatting for differnet content types
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

            val renderer = org.commonmark.renderer.html.HtmlRenderer.builder().build();
            renderer.render(ast);  // "<p>This is <em>Sparta</em></p>\n"
          }

          res
        }
        case TEXT => content
        case JSON | SCALA | JS => "<pre>" + content.replaceAll("\n", "<br/>") + "</pre>"
        case XML | HTML => content
        case _ => "UNKNOWN_MARKUP " + markup + " - " + content
      }
    } catch {
      case e : Throwable => {
        Audit.logdbWithLink("ERR_FORMATTING", wid.ahref, "[[ERROR FORMATTING]]: " + wid.wpath + " err: " + e.toString)
        log("[[ERROR FORMATTING]]: ", e)
        if(Services.config.isLocalhost) throw e
        "[[ERROR FORMATTING]] - sorry, dumb program here! The content is not lost: try editing this topic... also, please report this topic with the error and we'll fix it for you!"
      }
    }
    res
  }


  def prepUrl (url:String) = {
    if(Services.config.isDevMode && Services.config.isLocalhost)
      url
        .replace("http://cdn.razie.com/", "/admin/img/Users/raz/w/razie.github.io/")
        .replace("https://cdn.razie.com/", "/admin/img/")
//        .replace("https://cdn.razie.com/", "http://localhost:9000/asset/../../")
//        .replace("https://cdn.razie.com/", "file://Users/raz/w/razie.github.io/")
    else url
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

  /** a list to html */
  def toUl (res:List[Any]) =
    "<ul>" +
      res.take(100).map { x: Any =>
        "<li>" + x.toString + "</li>"
      }.mkString +
      (if(res.size>100)"<li>...</li>" else "") +
      "</ul>"

  def runXp(what: String, w: WikiEntry, path: String) = {
    val res = irunXp(what, w, path)

    what match {
      case "xp" => res.headOption.getOrElse("?").toString
      case "xpl" => toUl(res)
//      case "xmap" => res.take(100).map { x: Any => "<li>" + x.toString + "</li>" }.mkString
    }
    //        else "TOO MANY to list"), None))
  }

  // scaled down formatting of jsut some content
  def sformat(content: String, markup:String="md", realm:String, user:Option[WikiUser]=None) =
    format (WID("1","2").r(realm), markup, content, None, user)

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
  def format(we: WikiEntry, user:Option[WikiUser]) : String = {
    format (we.wid, we.markup, "", Some(we), user)
  }

  WikiObservers mini {
    case ev@WikiEvent(action, "WikiEntry", _, entity, _, _, _) => {
      action match {

        case WikiAudit.UPD_RENAME => {
          val oldWid = ev.oldId.flatMap(WID.fromPath)
          Wikis.clearCache(oldWid.get)
        }

        case a if WikiAudit.isUpd(a) => {
          val wid = WID.fromPath(ev.id)
          Wikis.clearCache(wid.get)
        }
        case _ => {}
      }
    }
  }

  /** clearing all possible versions of this WID from the cache */
  def clearCache(wids : WID*) = {
    wids.foreach(wid=>
      Array(
      wid.r("rk"), // yea, stupid but...
      wid,
      wid.copy(parent=None, section=None),
      wid.copy(realm = None, section=None),
      wid.copy(realm = None, parent=None, section=None),
      wid.copy(realm = None, parent=None, section=None, cat="")
    ).foreach {wid=>
      val key = wid.wpathFull
      WikiCache.remove(key + ".db")
      WikiCache.remove(key + ".formatted")
      WikiCache.remove(key + ".page")
    })
  }

  /** main formatting function
   *
   * @param wid - the wid being formatted
   * @param markup - markup language being formatted
   * @param icontent - the content being formatted or "" if there is a WikiEntry being formatted
   * @param we - optional page for context for formatting
   * @return
   */
  def format(wid: WID, markup: String, icontent: String, we: Option[WikiEntry], user:Option[WikiUser]) : String = {
    if (JSON == wid.cat || JSON == markup || XML == wid.cat || XML == markup || TEXT == markup)
      formatJson(wid, markup, icontent, we)
    else {
      var res = {
        val cacheFormatted = Services.config.cacheFormat

        if(cacheFormatted &&
          we.exists(w=> w.cacheable && w.category != "-" && w.category != "") &&
          (icontent == null || icontent == "") &&
          wid.section.isEmpty) {

          WikiCache.getString(we.get.wid.wpathFull+".formatted").map{x=>
            x
          }.getOrElse {
            val n = format1(wid, markup, icontent, we, user)
            if(we.exists(_.cacheable)) // format can change cacheable
              WikiCache.set(we.get.wid.wpathFull+".formatted", n, 300) // 10 miuntes
            n
          }
        } else
          format1(wid, markup, icontent, we, user)
      }

      // mark the external links
      val sup = "" //"""<sup>&nbsp;<b style="color:darkred">^</b></sup>""")
      val A_PAT = """(<a +href="http://)([^>]*)>([^<]*)(</a>)""".r
      res = A_PAT replaceSomeIn (res, { m =>
        if (Option(m group 2) exists (s=> !s.startsWith(Services.config.hostport)  &&
          !Services.isSiteTrusted("", s))
          )
          Some("""$1$2 title="External site"><i>$3</i>"""+sup+"$4")
        else None
      })

      // replace all divs - limitation of the markdown parser
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
        | <script>
        |  withJquery(function(){
        |   \$("#$1").attr("src","$2");
        |  });
        | </script>
        | """.stripMargin)
    y
  }

  // todo protect this from tresspassers
  def runScript(s: String, lang:String, page: Option[WikiEntry], au:Option[WikiUser]) = {
    // page preprocessed for, au or default to thread statics - the least reliable
    val up = page.flatMap(_.ipreprocessed.flatMap(_._2)) orElse au
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
        var extraParms = Map.empty[String,String]
        val TIF = """(?s)\{\{\.*(tif)([: ])?([^ :}]*)([ :]+)?([^}]+)?\}\}((?>.*?(?=\{\{/[^`])))\{\{/\.*tif\}\}""".r
        var res = TIF.replaceAllIn(c, { m =>
          if(parms.get(m.group(3)).exists(_.length > 0)) "$6"
          else if(m.group(5) != null) { // default value
            extraParms = extraParms + (m.group(3) -> m.group(5))
            "$6"
          } else ""
        })

        val s1 = (parms ++ extraParms).foldLeft(res){(a,b)=>
          a.replaceAll("\\{\\{\\$\\$"+b._1+"\\}\\}", b._2)
        }
      s1.replaceAll("\\{\\{`", "{{").replaceAll("\\[\\[`", "[[")
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
    we.urlRelative + (if (!shouldCount) "?count=0" else "")

  /** make a relative href for the given tag. give more tags with 1/2/3 */
  def hrefTag(wid:WID, t:String,label:String) = {
    if(Array("Blog","Forum") contains wid.cat) {
      s"""<b><a href="${w(wid)}/tag/$t">$label</a></b>"""
    } else {
      if(wid.parentWid.isDefined) {
        s"""<b><a href="${w(wid.parentWid.get)}/tag/$t">$label</a></b>"""
      } else {
        s"""<b><a href="/tag/$t">$label</a></b>"""
      }
    }
  }

  /////////////////// visibility for new wikis

  def mkVis(wid:WID, realm:String) = wid.findParent
    .flatMap(_.props.get("visibility"))
    .orElse(WikiReactors(realm).props.prop("default.visibility"))
    .getOrElse(
      WikiReactors(realm)
        .wiki
        .visibilityFor(wid.cat)
        .headOption
        .getOrElse(PUBLIC))

  /** extract wvis (edit permissions) prop from wiki */
  protected def wvis(props: Option[Map[String, String]]): Option[String] =
    props.flatMap(p => p.get("wvis").orElse(p.get("visibility"))).map(_.asInstanceOf[String])

  def mkwVis(wid:WID, realm:String) = wvis(wid.findParent.map(_.props))
    .orElse(WikiReactors(realm).props.prop("default.wvis"))
    .getOrElse(
      WikiReactors(realm)
        .wiki
        .visibilityFor(wid.cat)
        .headOption
        .getOrElse(PUBLIC))

  /** see if a exists otherwise return b */
  def fallbackPage (a:String, b:String) : String = {
    WID.fromPath(a).flatMap(find).map(x => a).getOrElse(b)
  }
}


