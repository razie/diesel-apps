/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.parser

import org.bson.types.ObjectId
import razie.wiki.mods.WikiMods
import razie.wiki.{Services, Enc}
import razie.{cdebug, cout, clog}

import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.util.matching.Regex.Match
import scala.util.parsing.combinator.RegexParsers
import scala.Option.option2Iterable
import scala.collection.mutable
import razie.wiki.model._

/** basic wiki parser - this is a trait so you can mix it in, together with other parser extensions, into your own parser
  *
  * the major patterns recognized are:
  *
  * - markdown extensions
  * - escape most html tags
  * \[\[link to other wiki page\]\]
  * \[\[\[link to wikipedia page\]\]\]
  * {{markup and properties}}
  * {{{magic markup and properties}}}
  * .keyword markup   // dot first char
  * {{xxx multiline markup}}
  * ...
  * {{/xxx}}
  * ::beg multiline markup
  * ...
  * ::end
  *
  *
  * Each parsing rule creates a SState, which are flattened at the end to both update the WikiEntry as well as create the
  * String representation of it - so you can be lazy in the SState.
  */
trait WikiParserT extends WikiParserBase with CsvParser {
  import WAST._
  
  def apply(input: String) = {
//    clog << ("PARSE: --------------------------")
//    clog << ("PARSE: "+input.lines.zipWithIndex.map(t=>(t._2, t._1)).mkString("\n"))
//    clog << ("PARSE: --------------------------")
    parseAll(wiki, input) match {
      case Success(value, _) => value
      // don't change the format of this message
      case NoSuccess(msg, next) => SState(s"[[CANNOT PARSE]] [${next.pos.toString}] : ${msg}")
    }
  }
//  def applys(input: String) = apply(input).s

  /** use this to expand [[xxx]] on the spot */
  def parseW2(input: String) = parseAll(wiki2, input) getOrElse SState("[[CANNOT PARSE]]")

  /** use this to parse wiki markdown on the spot - it is meant for short strings within like a cell or something */
  def parseLine(input: String) = parseAll(line, input) getOrElse SState("[[CANNOT PARSE]]")

  //============================== wiki parsing

  def wiki: PS = lines | line | xCRLF2 | xNADA

  def line: PS = opt(hr | lists) ~ rep(escaped1 | escaped | badHtml | badHtml2 | wiki3 | wiki2 | link2 | link1 | wikiProps | lastLine | linkUrl | xstatic) ^^ {
    case ol ~ l => ol.toList ::: l
  }

  def optline: PS = opt(dotProps | line) ^^ { case o => o.map(identity).getOrElse(SState.EMPTY) }

  private def TSNB : PS = "^THISSHALTNOTBE$" ^^ { case x => SState(x) }
  private def blocks : PS = moreBlocks.fold(TSNB)((x,y) => x | y)

  def lines: PS = rep((blocks ~ CRLF2) | (optline ~ (CRLF1 | CRLF3 | CRLF2))) ~ opt(dotProps | line) ^^ {
    case l ~ c =>
      l.map(t => RState("", t._1, t._2)) ::: c.toList
  }

  def wiki3: PS = "[[[" ~ """[^]]*""".r ~ "]]]" ^^ {
    case "[[[" ~ name ~ "]]]" => """<a href="http://en.wikipedia.org/wiki/%s"><i>%s</i></a>""".format(name, name)
  }

  def wiki2: PS = "[[" ~ """[^]]*""".r ~ "]]" ^^ {
    case "[[" ~ name ~ "]]" => {
      val p = parseAll(wikiPropsRep, name)
      if (p.successful) {
        // this is an ilink with auto-props in the name/label
        // for now, reformat the link to allow props and collect them in the ILInk
        //        SedWiki(wikip2a, expand2 _, identity, p.get.s).map(x => SState(x._1, Map(), x._2.map(x => ILink(x.cat, x.name, x.label, p.get.tags, p.get.ilinks)).toList)).get // TODO something with the props
        SedWiki(realm, identity, p.get.s).map(x => SState(x._1, Map(), x._2.map(x => ILink(x.wid, x.label, x.role, p.get.tags, p.get.ilinks)).toList)).get // TODO something with the props
      } else {
        // this is a normal ilink
        //        SedWiki(wikip2a, expand2 _, Wikis.formatName _, name).map(x => SState(x._1, Map(), x._2.toList)).get
        SedWiki(realm, Wikis.formatName _, name).map(x => SState(x._1, Map(), x._2.toList)).get
      }
    }
  }

  final val wikip2a = """([^:|\]]*::)?([^:|\]]*:)?([^/:|\]]*[/:])?([^|\]]+)([ ]*[|][ ]*)?([^]]*)?"""

  // just leave MD []() links alone
  def link2: PS = """\[[^\]]*\]\([^)]+\)""".r ^^ {
    case a => a
  }

  //  simplified links with [xxx text] - uses only links starting with http or / or .
    def link1: PS = "[" ~ not("[") ~> ("""http[s]?://""".r | "/") ~ """[^] <>]*""".r ~ opt("[ ]+".r ~ """[^]]*""".r) <~ "]" ^^ {
    case http ~ url ~ Some(sp ~ text) => """<a href="%s">%s</a>""".format(http+url, text)
    case http ~ url ~ None => """<a href="%s">%s</a>""".format(http+url, url)
//      case http ~ url ~ Some(sp ~ text) => """[%s](%s)""".format(text, http+url)
//      case http ~ url ~ None => """[%s](%s)""".format(url, http+url)
    }

//    simplified links with [xxx text]
//  def link1: PS = "[" ~ not("[" | "\"") ~> opt("""http[s]?://""".r) ~ """[^] ]*""".r ~ opt("[ ]+".r ~ """[^]]*""".r) <~ "]" ^^ {
//    case http ~ url ~ Some(sp ~ text) => """[%s](%s)""".format(text, http.getOrElse("")+url)
//    case http ~ url ~ None => """[%s](%s)""".format(url, http.getOrElse("")+url)
//  }


  /** simplify url - don't require markup for urls so http://xxx works */
  def linkUrl: PS = not("""[\[(]""".r) ~> """http[s]?://""".r ~ """[^\s\]]+""".r ^^ {
//    case http ~ url => s"""[$http$url]($http$url)"""
    case http ~ url => s"""<a href="$http$url">$http$url</a>"""
  }

  def hr: PS = """^---+""".r ~> opt("div:" ~> ".*") ^^ {
    case div => "<hr>"
  }

  /** quick list element */
  def lists = li1 | li2 | li3
  def li1: PS = """^ [*-] """.r ^^ { case _ => "    * " }
  def li2: PS = """^  [*-] """.r ^^ { case _ => "        * " }
  def li3: PS = """^   [*-] """.r ^^ { case _ => "            * " }

  def iframe: PS = "<iframe" ~> """[^>]*""".r <~ ">" <~ opt(""" *</iframe>""".r) ^^ {
    case a => {
      val url = a.replaceAll(""".*src="(.*)".*""", "$1")
      val x = Services.config.config(Services.config.SAFESITES).flatMap(_.keys.find(x => url.startsWith(x)).map(x => "<iframe" + a + "></iframe>")) getOrElse ("&lt;iframe" + a + "&gt;")
      x
    }
  }

  private def badHtml: PS = iframe | badTags

  private def badTags: PS = "<" ~> ParserSettings.hnok.r ~ opt(" " ~ """[^>]*""".r) <~ ">" ^^ {
    case b ~ Some(c ~ d) => "&lt;" + b + c + d + "&gt;"
    case b ~ None => "&lt;" + b + "&gt;"
  }

  private def badHtml2: PS = "</" ~> ParserSettings.hnok.r <~ ">" ^^ {
    case b => "&lt;/" + b + "&gt;"
  }

  //======================= {{name:value}}

  // this is used when matching a link/name
  private def wikiPropsRep: PS = rep(wikiPropMagicName | wikiPropByName | wikiPropWhenName |
    wikiPropWhereName | wikiPropLocName | wikiPropRoles | wikiProp |
    xstatic) ^^ {
    // LEAVE this as a SState - don't make it a LState or you will have da broblem
    case l => SState(l.map(_.s).mkString, l.flatMap(_.tags).toMap, l.flatMap(_.ilinks))
  }

  // this is used for contents of a topic
  private def wikiProps: PS =
    moreWikiProps.foldLeft(
    wikiPropMagic | wikiPropBy | wikiPropWhen | wikiPropXp | wikiPropWhere |
    wikiPropLoc | wikiPropRoles | wikiPropAttrs | wikiPropAttr | wikiPropWidgets | wikiPropCsv | wikiPropCsv2 |
    wikiPropTable | wikiPropISection | wikiPropSection | wikiPropImg | wikiPropVideo |
    wikiPropCode | wikiPropField | wikiPropRk | wikiPropLinkImg | wikiPropFeedRss | wikiPropTag |
    wikiPropRed | wikiPropLater
    )((x,y) => x | y) | wikiProp

  private def wikiPropMagic: PS = "{{{" ~> """[^}]*""".r <~ "}}}" ^^ {
    case value => {
      val p = parseAll(dates, value)
      if (p.successful) {
        SState("""{{Date %s}}""".format(value), Map("date" -> value))
      } else {
        SState("""{{??? %s}}""".format(value), Map("magic" -> value))
      }
    }
  }
  def wikiPropMagicName: PS = """{{{""" ~> """[^}]*""".r <~ """}}}""" ^^ {
    case value => {
      val p = parseAll(dates, value)
      if (p.successful) {
        SState("""{{date %s}}""".format(value), Map("date" -> value))
      } else {
        SState("""{{??? %s}}""".format(value), Map("magic" -> value))
      }
    }
  }

  def wikiProp: PS = "{{" ~> """[^}: ]+""".r ~ """[: ]""".r ~ """[^}]*""".r <~ "}}" ^^ {
    case name ~ _ ~ value =>
      // default to widgets
      if(Wikis(realm).index.containsName("widget_" + name.toLowerCase()))
        SState(
          // todo cache this
          Wikis(realm).find(WID("Admin", "widget_" + name.toLowerCase())).map(_.content).map { c =>
            List(("WIDGET_ARGS", value)).foldLeft(c)((c, a) => c.replaceAll(a._1, a._2))
          } getOrElse "")
      else if(WikiMods.index.contains(name.toLowerCase)) {
        LazyState {(current, we) =>
          //todo the mod to be able to add some properties in the context of the current topic
          SState(WikiMods.index(name.toLowerCase).modProp(name, value, we))
        }
      } else {
        if (name startsWith ".")
          SState("", Map(name.substring(1) -> value)) // hidden
        else
          SState(s"""<span style="font-weight:bold">{{Property $name=$value}}</span>\n\n""", Map(name -> value))
      }
  }

  private def dotProps: PS = moreDotProps.foldLeft(dotPropTags | dotPropName )((x,y) => x | y) | dotProp

  def dotProp: PS = """^\.""".r ~> """[.]?[^.: ][^: ]+""".r ~ """[: ]""".r ~ """[^\r\n]*""".r ^^ {
    case name ~ _ ~ value =>

      // default to widgets
      if(Wikis(realm).index.containsName("widget_" + name.toLowerCase()))
        SState(
          // todo cache this
          Wikis(realm).find(WID("Admin", "widget_" + name.toLowerCase())).map(_.content).map { c =>
            List(("WIDGET_ARGS", value)).foldLeft(c)((c, a) => c.replaceAll(a._1, a._2))
          } getOrElse "")
      else {
        if (name startsWith ".")
          SState ("", Map (name.substring (1) -> value) ) // hidden
        else
          SState (s"""<span style="font-weight:bold">{{Property $name=$value}}</span>\n\n""", Map (name -> value) )
      }
  }

  def dotPropTags: PS = """^\.t """.r ~> """[^\n\r]*""".r  ^^ {
    case value => SState("", Map("inlinetags" -> value)) // hidden
  }

  def dotPropName: PS = """^\.n """.r ~> """[^\n\r]*""".r  ^^ {
    case value => SState(s"""<small><span style="font-weight:bold;">$value</span></small><br>""", Map("name" -> value))
  }

  private def wikiPropByName: PS = ("\\{\\{[Bb]y[: ]+".r | "\\{\\{[Cc]lub[: ]+".r) ~> """[^}]*""".r <~ "}}" ^^ {
    case place => SState("""{{by %s}}""".format(place), Map("by" -> place))
  }

  private def wikiPropBy: PS = ("\\{\\{[Bb]y[: ]+".r | "\\{\\{[Cc]lub[: ]+".r) ~> """[^}]*""".r <~ "}}" ^^ {
    case place => SState("{{by " + parseW2("""[[Club:%s]]""".format(place)).s + "}}", Map("club" -> place), ILink(WID("Club", place), place) :: Nil)
  }

  private def wikiPropWhere: PS = ("\\{\\{where[: ]".r | "\\{\\{[Aa]t[: ]+".r | "\\{\\{[Pp]lace[: ]+".r | "\\{\\{[Vv]enue[: ]+".r) ~> """[^}]*""".r <~ "}}" ^^ {
    case place => SState("{{at " + parseW2("""[[Venue:%s]]""".format(place)).s + "}}", Map("venue" -> place), ILink(WID("Venue", place), place) :: Nil)
  }

  private def wikiPropWhereName: PS = ("\\{\\{where[: ]".r | "\\{\\{[Aa]t[: ]+".r | "\\{\\{[Pp]lace[: ]+".r | "\\{\\{[Vv]enue[: ]+".r) ~> """[^}]*""".r <~ "}}" ^^ {
    case place => SState("""{{at %s}}""".format(place), Map("venue" -> place), ILink(WID("Venue", place), place) :: Nil)
  }

  private def wikiPropLoc: PS = "{{" ~> "loc" ~> """[: ]""".r ~> """[^}:]*""".r ~ ":".r ~ """[^}]*""".r <~ "}}" ^^ {
    case what ~ _ ~ loc => {
      if ("ll" == what)
        SState("""{{[Location](http://maps.google.com/maps?ll=%s&z=15)}}""".format(loc), Map("loc" -> (what + ":" + loc)))
      else if ("s" == what)
        SState("""{{[Location](http://www.google.com/maps?hl=en&q=%s)}}""".format(loc.replaceAll(" ", "+")), Map("loc" -> (what + ":" + loc)))
      else if ("url" == what)
        SState("""{{[Location](%s)}}""".format(loc), Map("loc" -> (what + ":" + loc)))
      else
        SState("""{{Unknown location spec: %s value %s}}""".format(what, loc), Map("loc" -> (what + ":" + loc)))
    }
  }

  private def wikiPropLocName: PS = "{{" ~> "loc" ~> """[: ]""".r ~> """[^:]*""".r ~ ":".r ~ """[^}]*""".r <~ "}}" ^^ {
    case what ~ _ ~ loc => {
      SState("""{{at:%s:%s)}}""".format(what, loc), Map("loc:" + what -> loc))
    }
  }

  private def wikiPropWhen: PS = ("\\{\\{when[: ]".r | "\\{\\{[Oo]n[: ]+".r | "\\{\\{[Dd]ate[: ]+".r) ~> """[^}]*""".r <~ "}}" ^^ {
    case date => {
      val p = parseAll(dates, date)
      if (p.successful) {
        SState("""{{Date %s}}""".format(date), Map("date" -> date))
      } else {
        SState("""{{Date ???}}""".format(date), Map())
      }
    }
  }

  private def wikiPropWhenName: PS = ("\\{\\{when[: ]".r | "\\{\\{[Oo]n[: ]+".r | "\\{\\{[Dd]ate[: ]+".r) ~> """[^}]*""".r <~ "}}" ^^ {
    case date => {
      val p = parseAll(dates, date)
      if (p.successful) {
        SState(s"""{{date $date}}""", Map("date" -> date))
      } else {
        SState(s"""$date??""", Map())
      }
    }
  }

  private def wikiPropXp: PS = "{{" ~> """xpl?""".r ~ """[: ]""".r ~ """[^}]*""".r <~ "}}" ^^ {
    case what ~ _ ~ path => {
      SState(s"""`{{{$what:$path}}}`""", Map())
    }
  }

  // reused
  private def a(name:String, kind:String,d:String="") =
    SState(s"Attr: <b>$name</b>", Map("attr:" + name -> kind))

  private def wikiPropAttrs: PS = "{{attrs" ~> """[: ]""".r ~> """[^:}]*""".r <~ "}}" ^^ {
    case names => {
      LState(SState("Attrs:") :: names.split(",").map(name=>a(name, "")).toList )
    }
  }

  private def wikiPropAttr: PS = "{{attr" ~> """[: ]""".r ~> """[^:}]*""".r ~ opt(":".r ~> """[^}]*""".r) <~ "}}" ^^ {
    case name ~ kind => {
      a (name, kind.getOrElse(""))
    }
  }

  private def wikiPropRoles: PS = "{{roles" ~> """[: ]""".r ~> """[^:]*""".r ~ ":".r ~ """[^}]*""".r <~ "}}" ^^ {
    case cat ~ _ ~ how => {
      val r = if(Wikis.RK == realm) "Category" else realm+".Category"
      val cats = "<b>"+parseW2(s"[[$r:$cat | $cat]]").s+"</b>"

      SState(
        how match {
          case "Child"     =>  s"{{Has $cats(s)}}"
          case "Parent"    =>  s"{{Owned by $cats(s)}}"
          case "Spec"      =>  s"{{Specified by $cats(s)}}"
          case "SpecFor"   =>  s"{{Specification for $cats(s)}}"
          case "Assoc"     =>  s"{{Associated to $cats(s) as $how}}"
          case _           =>  s"{{Can link from $cats(s) as $how}}"
        },
        Map("roles:" + cat -> how))
    }
  }

  // just a nice badge for RK
  private def wikiPropRk: PS = "{{" ~> ("rk" | "wiki") ~ opt("[: ]".r ~> """[^}]*""".r) <~ "}}" ^^ {
    case rk ~ what => {
      rk match {
        case "rk" =>  what match {
          case Some("member") =>
            SState("""<a class="badge badge-warning" href="http://www.racerkidz.com/wiki/Admin:Member_Benefits">RacerKidz</a>""")
          case Some("club") =>
            SState("""<a class="badge badge-warning" href="http://www.racerkidz.com/wiki/Admin:Club_Hosting">RacerKidz</a>""")
          case _ =>
            SState("""<a class="badge badge-warning" href="http://www.racerkidz.com">RacerKidz</a>""")
        }
        case "wiki" =>
          SState("""<a class="badge badge-warning" href="http://www.wikireactor.com">WikiReactor</a>""")
        case _ =>
          SState("""<a class="badge badge-warning" href="http://www.wikireactor.com">WikiReactor</a>""")
      }
    }
  }

  private def wikiPropRed: PS = "{{" ~> "red" ~> opt("[: ]".r ~> """[^}]*""".r) <~ "}}" ^^ {
    case what => SState(s"""<span style="color:red;font-weight:bold;">${what.mkString}</span>""")
  }

  private def wikiPropLater: PS = "{{" ~> "later" ~> "[: ]".r ~> """[^ :]*""".r ~ "[: ]".r ~ """[^}]*""".r <~ "}}" ^^ {
    case id~ _ ~ url => SState(s"""<script async>$$("#$id").load("$url");</script>""")
  }

  private def wikiPropWidgets: PS = "{{" ~> "widget:" ~> "[^:]+".r ~ optargs <~ "}}" ^^ {
    case name ~ args => {
      SState(
      // todo cache this
        Wikis(realm).find(WID("Admin", "widget_" + name)).map(_.content).map { c =>
          args.foldLeft(c)((c, a) => c.replaceAll(a._1, a._2))
        } getOrElse "")
    }
  }

  //======================= forms

  private def wikiPropField: PS = "{{" ~> "f:" ~> "[^:]+".r ~ optargs <~ "}}" ^^ {
    case name ~ args => {
      SState(
        "`{{{f:%s}}}`".format(name), Map(), List(), List({ w =>
          w.fields = w.fields ++ Map(name -> FieldDef(name, "", args.map(t => t).toMap))
          w
        }))
    }
  }

  //todo what was this for? - it's not used
  private def wikiPropFieldVal: PS = "{{" ~> "fval:" ~> "[^:]+".r ~ optargs <~ "}}" ^^ {
    case name ~ args => {
      SState(
        Wikis(realm).find(WID("Admin", "widget_" + name)).map(_.content).map { c =>
          args.foldLeft(c)((c, a) => c.replaceAll(a._1, a._2))
        } getOrElse "")
    }
  }

  // to not parse the content, use slines instead of lines
  /** {{section:name}}...{{/section}} */
  def wikiPropSection: PS = "{{" ~> opt(".") ~ """section|template|properties""".r ~ "[: ]".r ~ """[^}]*""".r ~ "}}" ~ lines <~ ("{{/" ~ """section|template|properties""".r ~ "}}") ^^ {
    case hidden ~ stype ~ _ ~ name ~ _ ~ lines => {
      val sname = "{{" + stype + ":" + name + "}}"
      hidden.map(x => SState.EMPTY) getOrElse SState(
        "`SECTION START " + sname + "`<br>") + lines + SState("<br>`SECTION END` " + sname )
    }
  }

  // to not parse the content, use slines instead of lines
  def wikiPropISection: PS = "{{`" ~> opt(".") ~ """section|template|properties""".r ~ ":" ~ """[^}]*""".r ~ "}}" ~ slines ~ ("{{/" ~ "`" ~ """section|template|properties""".r ~ "}}") ^^ {
    case hidden ~ stype ~ _ ~ name ~ _ ~ lines ~ (e1 ~ e2 ~ e3 ~ e4) => {
      val sname = "{{`" + hidden.mkString + stype + ":" + name + "}}"
      RState(sname, lines, e1+e2+e3+e4)
    }
  }

  // let it be - avoid replacing it - it's expanded in Wikis where i have the wid
  def wikiPropTag: PS = "{{tag" ~ """[: ]""".r ~> """[^}]*""".r <~ "}}" ^^ {
    case name => {
      SState(s"""`{{tag:$name}}`""")
    }
  }

  def wikiPropImg: PS = "{{img" ~> opt("""\.icon|\.small|\.medium""".r) ~ """[: ]""".r ~ """[^} ]*""".r ~ optargs <~ "}}" ^^ {
    case stype ~ _ ~ name ~ args => {
      val sargs = args.foldLeft(""){(c, a) => s""" $c ${a._1}="${a._2}" """}
      SState(s"""<img src="$name" $sargs />""")
    }
  }

  private def wikiPropVideo: PS = "{{" ~> ("video" | "photo" | "slideshow") ~ """[: ]""".r ~ """[^}]*""".r <~ "}}" ^^ {
    case what ~ _ ~ url => wpVideo(what, url)
  }

  private def wpVideo (what:String, url:String) = {
    what match {
      case "video" => {
        val yt1 = """http[s]?://youtu.be/([^?]+)(\?t=.*)?""".r
        val yt2 = """http[s]?://www.youtube.com/watch?.*v=([^?&]+).*""".r
        val vm3 = """http[s]?://vimeo.com/([^?&]+)""".r
        def xt(id: String, time:Option[String] = None) = s"""<iframe width="560" height="315" src="http://www.youtube.com/embed/$id${time.map(x=>"?start="+x).mkString}" frameborder="0" allowfullscreen></iframe>"""//.format(id)
        def vm(id: String) = """<iframe src="//player.vimeo.com/video/%s" width="500" height="281" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>""".format(id)
        url match {
          case yt1(a, g1) => {
            if(g1 == null) { // no time info
              SState(xt(a))
            } else {
              // turn 1m1s into 61
              val ts = """\?t=([0-9]+m)?([0-9]+s)?""".r
              val ts(g2,g3) = g1
              val sec = (if(g2 == null) 0 else g2.substring(0,g2.length-1).toInt * 60) + (if(g3 == null) 0 else g3.substring(0,g3.length-1).toInt)
              SState(xt(a, Option(if(sec == 0) null else sec.toString)))
            }
          }
          case yt2(a) => SState(xt(a))
          case vm3(a) => SState(vm(a))
          case _ => SState("""{{Unsupported video source - please report to support: <a href="%s">url</a>}}""".format(url))
        }
      }
      case "photo" => {
        val yt2 = """(.*)""".r
        def xt2(id: String) = """<a href="%s"><img src="%s"></a>""".format(id, id)
        url match {
          case yt2(a) => SState(xt2(a))
          case _ => SState("""{{Unsupported photo source - please report to support: [%s]}}""".format(url))
        }
      }
      case "slideshow" => {
        val yt1 = """(.*)""".r
        def xt(id: String) = """<a href="%s">Slideshow</a>""".format(id)
        url match {
          case yt1(a) => SState(xt(a))
          case _ => SState("""{{Unsupported slideshow source - please report to support: %s}}""".format(url))
        }
      }
    }
  }

  // todo more humane name
  def wikiPropLinkImg: PS = "{{link.img" ~ "[: ;]".r ~> """[^ ;}]*""".r ~ "[: ;]".r ~ """[^ ;}]*""".r <~ "}}" ^^ {
    case url ~ _ ~ link => {
      s"""<a href="$link"><img src="$url" /></a>"""
    }
  }

  // todo more humane name
  def wikiPropFeedRss: PS = "{{feed.rss" ~ "[: ;]".r ~> """[^ ;}]*""".r <~ "}}" ^^ {
    case xurl => {
      import razie.Snakk._
      val rss  = url (xurl)
      Try {
        (for (xn <- xml(scala.xml.XML.loadString(body(rss))) \ "channel" \ "item") yield {
          val n = xml(xn)
          // insulate external strings - sometimes the handling of underscore is bad
          // replace urls with MD markup:
          def ext(s: String) = s //.replaceAll("""(\s|^)(https?://[^\s]+)""", "$1[$2]")

          val link = ext(n \@ "link")
          val title = ext(n \@ "title")
          val desc = ext(n \@ "description")
          s"""<h3><a href="$link">$title</a></h3><pre>$desc</pre> """
        }).mkString
      }
    }.recover{
      case e:Throwable => s"Error reading RSS: ${e.getClass.getSimpleName} : ${e.getLocalizedMessage}"
    }.get
  }

  def wikiPropCode: PS = "{{" ~> """code""".r ~ "[: ]".r ~ """[^:}]*""".r ~ "}}" ~ opt(CRLF1 | CRLF3 | CRLF2) ~ slines <~ "{{/code}}" ^^ {
    case stype ~ _ ~ name ~ _ ~ crlf ~ lines => {
      RState("<pre><code>", lines, "</code></pre>")
    }
  }

  //======================= delimited imports and tables

  def wikiPropCsv: PS = "{{" ~> "r1.delimited:" ~> (wikiPropCsvStart >> { h: CsvHeading => csv(h.delim) ^^ { x => (h, x) } }) <~ "{{/r1.delimited}}" ^^ {
    case (a, body) => {
      val c = body
      //      SState(a.s) + c.filter(_.size > 0).map { l =>
      //        SState("\n* ") + parseW2("[[" + a.what + ":" + l.zip(a.h).filter(c => c._1.length > 0).map {c =>
      //          if ("_" == c._2) c._1 else "{{" + c._2 + " " + c._1 + "}}"
      //        }.mkString(" ") + "]]")
      //      }.reduce(_ + _) + "\n"
      RState(a.s,
        c.filter(_.size > 0).map { l =>
          RState(
            "\n* ",
            parseW2("[[" + a.what + ":" + l.zip(a.h).filter(c => c._1.length > 0).map {c =>
              if ("_" == c._2) c._1 else "{{" + c._2 + " " + c._1 + "}}"
            }.mkString(" ") + "]]"),
            "")
        },
        "\n")
    }
  }

  def wikiPropCsv2: PS = "{{" ~> "r1.delimited2:" ~> """[^:]*""".r ~ ":" ~ """[^:]*""".r ~ ":" ~ (wikiPropTableStart >> { h: CsvHeading => csv(h.delim) ^^ { x => (h, x) } }) <~ "{{/r1.delimited}}" ^^ {
    case prefix ~ _ ~ cat ~ _ ~ Tuple2(a, body) => {

      def ecell(cat: String, p: String, a: String, b: String) =
        parseW2("[[" + cat + ":" + p + " " + a + " " + b + "]]")

      RState(a.s,
        body.map(l =>
          if (l.size > 0) RState(
            "\n<tr>",
            l.map{c => RState(
              "<td>" + c + "</td>",
              a.h.tail.map(b => RState("<td>", ecell(cat, prefix, c, b), "</td>")),
              "")},
            "</tr>")
          else SState.EMPTY),
        "\n</table>")
    }
  }

  def wikiPropTable: PS = "{{" ~> "r1.table:" ~> (wikiPropTableStart >> { h: CsvHeading => csv(h.delim) ^^ { x => (h, x) } }) <~ "{{/r1.table}}" ^^ {
    case (a, body) => {
      RState(a.s,
        body.map(l =>
          if (l.size > 0) RState(
            "\n<tr>",
            l.map(c => RState("<td>", parseLine(c), "</td>")),
            "</tr>")
          else SState.EMPTY),
        "\n</table>")
    }
  }

  case class CsvHeading(what: String, s: String, delim: String = ";", h: List[String] = Nil)

  def heading: P = (not("}}") ~> not(",") ~> """.""".r+) ^^ { case l => l.mkString }

  def csvHeadings: Parser[CsvHeading] = heading ~ rep("," ~> heading) ^^ {
    case ol ~ l => CsvHeading("", "", "", List(ol) ::: l)
  }

  def wikiPropCsvStart: Parser[CsvHeading] = """.""".r ~ ":".r ~ """[^:]*""".r ~ opt(":".r ~ csvHeadings) <~ "}}" ^^ {
    case delim ~ _ ~ what ~ head => {
      var s = what + "(s):" + "\n"

      CsvHeading(what, s, delim, head.map(_._2.h).getOrElse(List()))
    }
  }

  /** delim and optional column headings */
  def wikiPropTableStart: Parser[CsvHeading] = """.""".r ~ ":".r ~ opt(csvHeadings) <~ "}}" ^^ {
    case delim ~ _ ~ head => {
      var s = """<table class="table table-striped">"""

      if (head.isDefined)
        s += "\n<thead><tr>" + head.get.h.map(e => "<th>" + e + "</th>").mkString + "</tr></thead>"

      CsvHeading("", s, delim, head.map(_.h).getOrElse(List()))
    }
  }


  //======================= dates
  import ParserSettings.{mth1, mth2}
  
  def dates = date1 | date2
  def date1 = """\d\d\d\d""".r ~ "-" ~ """\d\d""".r ~ "-" ~ """\d\d""".r ^^ { case y ~ _ ~ m ~ _ ~ d => "%s-%s-%s".format(y, m, d) }
  def date2 = (mth2 + "|" + mth1).r ~ " *".r ~ """\d[\d]?""".r ~ "[ ,-]*".r ~ """\d\d\d\d""".r ^^ { case m ~ _ ~ d ~ _ ~ y => "%s-%s-%s".format(y, m, d) }
}

/** delimited and table parser */
trait CsvParser extends ParserCommons {
  import WAST._
  
  def csv(implicit xdelim: String): Parser[List[List[String]]] = csvLines

  def csvCRLF2: PS2 = CRLF2 ^^ { case x => Nil }
  def csvNADA: PS2 = NADA ^^ { case x => Nil }

  def csvLine(implicit xdelim: String): PS1 = (cell | xdelim) ~ rep(xdelim ~ (cell | NADA)) ^^ {
    case ol ~ l => {
      if (ol == xdelim) List("") ::: l.map(_._2)
      else List(ol) ::: l.map(_._2)
    }
  }

  def csvLines(implicit xdelim: String): PS2 = rep(csvOptline ~ (CRLF1 | CRLF3 | CRLF2)) ~ opt(csvLine) ^^ {
    case l ~ c => (l.map(_._1) ::: c.toList)
  }

  def plainLines(implicit end: String): P = rep(opt(plainLine) ~ (CRLF1 | CRLF3 | CRLF2)) ~ opt(plainLine) ^^ {
    case l ~ c => l.flatMap(_._1.map(_ + "\n")).mkString + c.getOrElse("")
  }

  def plainLine(implicit end: String): P = (not(end) ~> """.""".r+) ^^ { case l => l.mkString }

  def csvOptline(implicit xdelim: String): PS1 = opt(csvLine) ^^ { case o => o.map(identity).getOrElse(Nil) }

  def cell(implicit xdelim: String): P = (not(xdelim) ~> not("{{") ~> not("}}") ~> """.""".r+) ^^ { case l => l.mkString }
  //  def cell(implicit xdelim: String): P = (not(xdelim) ~> not("{{") ~> not("[[") ~> not("}}") ~> """.""".r+) ^^ { case l => l.mkString }
}

//object Widgets {
//  def weather(s: String) = """
//<iframe marginheight="0" marginwidth="0" name="wxButtonFrame" id="wxButtonFrame" height="168"
//    src="http://btn.weather.ca/weatherbuttons/template7.php?placeCode=%s&category0=Cities&containerWidth=180&btnNo=&backgroundColor=blue&multipleCity=0&citySearch=1&celsiusF=C"
//    align="top" frameborder="0" width="180" scrolling="no"></iframe><br>
//""".format(s)
//}

