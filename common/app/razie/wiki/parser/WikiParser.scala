/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.parser

import java.net.URI

import org.bson.types.ObjectId
import razie.wiki.mods.WikiMods
import razie.wiki.{Sec, EncUrl, Services, Enc}
import razie.{cdebug, cout, clog}

import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.util.matching.Regex.Match
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.token.Tokens
import scala.Option.option2Iterable
import scala.collection.mutable
import razie.wiki.model._

/** minimal parser - use this instead of the WikiParserT if no wiki specific rules are needed,
  *
  * i.e. you jsut want to parse the DomParser rules
  */
trait WikiParserMini extends WikiParserBase with CsvParser with Tokens {

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

  def line: PS = opt(hr | lists) ~ rep(escaped2 | escaped1 | escaped | badHtml | badHtml2 | wiki3 | wiki2 | link2 | link1 | wikiProps | lastLine | linkUrl | xstatic) ^^ {
    case ol ~ l => ol.toList ::: l
  }

  def optline: PS = opt(escaped2 | dotProps | videoUrlOnaLine | line) ^^ { case o => o.map(identity).getOrElse(SState.EMPTY) }

  private def TSNB : PS = "^THISSHALTNOTBE$" ^^ { case x => SState(x) }
  private def blocks : PS = moreBlocks.fold(TSNB)((x,y) => x | y)

  def lines: PS = rep((blocks ~ CRLF2) | (optline ~ (CRLF1 | CRLF3 | CRLF2))) ~ opt(escaped2 | dotProps | videoUrlOnaLine | line) ^^ {
    case l ~ c =>
      l.map(t => t._1 match {
        // just optimizing to reduce the number of resulting elements
        //        case ss:SState => ss.copy(s = ss.s+t._2)
        case _ => RState("", t._1, t._2)
      }) ::: c.toList
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
        SedWiki(realm, identity, p.get.s).map(x => SState(x._1, Map(), x._2.map(x => ILink(x.wid, x.label, x.role, p.get.props, p.get.ilinks)).toList)).getOrElse(SState("")) // TODO something with the props
      } else {
        // this is a normal ilink
        //        SedWiki(wikip2a, expand2 _, Wikis.formatName _, name).map(x => SState(x._1, Map(), x._2.toList)).get
        SedWiki(realm, Wikis.formatName _, name).map(x => SState(x._1, Map(), x._2.toList)).getOrElse(SState(""))
      }
    }
  }

  //todo can't do it here - sections will include AST trees then not text
  //  def wiki2Include: PS = "[[include:" ~> """[^]]*""".r <~ "]]" ^^ {
  //    case wpath  => {
  //      val other = for (
  //        wid <- WID.fromPath(wpath);
  //        c <- wid.content // this I believe is optimized for categories
  //      ) yield c
  //
  //      other.map(apply).getOrElse(SState("`[ERR Can't include $1]`"))
  //    }
  //  }

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

  // if url is on a line, maybe it's a youtube vid and will expand.
  // use lookahead for newline with regex
  private def videoUrlOnaLine: PS = not("""[\[(]""".r) ~> """http[s]?://""".r ~ """[^\s\]]+\s*(?=[\r\n])""".r  ^^ {
    case http ~ urlx => {
      val url = urlx.trim
      // auto expand videos
      val yt1 = """http[s]?://youtu.be/([^?]+)(\?t=.*)?""".r
      val yt2 = """http[s]?://www.youtube.com/watch?.*v=([^?&]+).*""".r
      val vm3 = """http[s]?://vimeo.com/([^?&]+)""".r

      if(
        (http+url).matches(yt1.regex) ||
          (http+url).matches(yt2.regex) ||
          (http+url).matches(vm3.regex)
      ) wpVideo("video", http+url, Nil)
      else
        SState(s"""<a href="$http$url">$http$url</a>""")
    }
  }

  /** simplify url - don't require markup for urls so http://xxx works */
  private def linkUrl: PS = not("""[\[(]""".r) ~> """http[s]?://""".r ~ """[^\s\]]+""".r ^^ {
    case http ~ url =>
      SState(s"""<a href="$http$url">$http$url</a>""")
  }

  protected def wpVideo (what:String, url:String, args:List[(String,String)]) = {
    val caption = getCaption(args, "left")
    what match {
      case "video" => {
        val yt1 = """http[s]?://youtu.be/([^?]+)(\?t=.*)?""".r
        val yt2 = """http[s]?://www.youtube.com/watch?.*v=([^?&]+).*""".r
        val vm3 = """http[s]?://vimeo.com/([^?&]+)""".r
        val vp4 = """http[s]?://videopress.com/v/([^?&]+)""".r
        def xt(id: String, time:Option[String] = None) = s"""<iframe width="560" height="315" src="http://www.youtube.com/embed/$id${time.map(x=>"?start="+x).mkString}" frameborder="0" allowfullscreen></iframe>"""//.format(id)
        def vm(id: String) = """<iframe src="//player.vimeo.com/video/%s" width="500" height="281" frameborder="0" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>""".format(id)
        def vp(id: String) = s"""<iframe width="560" height="315" src="https://videopress.com/embed/$id" frameborder="0" allowfullscreen></iframe>
          <script src="https://videopress.com/videopress-iframe.js"></script>"""
        url match {
          case yt1(a, g1) => {
            if(g1 == null) { // no time info
              SState(xt(a)+s"<br>$caption<br>")
            } else {
              // turn 1m1s into 61
              val ts = """\?t=([0-9]+m)?([0-9]+s)?""".r
              val ts(g2,g3) = g1
              val sec = (if(g2 == null) 0 else g2.substring(0,g2.length-1).toInt * 60) + (if(g3 == null) 0 else g3.substring(0,g3.length-1).toInt)
              SState(xt(a, Option(if(sec == 0) null else sec.toString))+s"<br>$caption<br>")
            }
          }
          case yt2(a) => SState(xt(a)+s"<br>$caption<br>")
          case vm3(a) => SState(vm(a)+s"<br>$caption<br>")
          case vp4(a) => SState(vp(a)+s"<br>$caption<br>")
          case _ => SState("""{{Unsupported video source - please report to support: <a href="%s">url</a>}}""".format(url))
        }
      }
      case "slideshow" => {
        val yt1 = """(.*)""".r
        def xt(id: String) = """<a href="%s">Slideshow</a><br>""".format(id)
        url match {
          case yt1(a) => SState(xt(a)+s"<br>$caption<br>")
          case _ => SState("""{{Unsupported slideshow source - please report to support: %s}}""".format(url))
        }
      }
    }
  }

  def getCaption(iargs:List[(String,String)], align:String="center"):String = iargs.toMap.get("caption").map(x=>
    s"""
       |<div style="text-align: $align;">
       |<small style="align:$align">$x</small>
       |</div>
        """.stripMargin) getOrElse ""

  private def hr: PS = """^---+""".r ~> opt("div:" ~> ".*") ^^ {
    case div => "<hr>"
  }

  /** quick list element */
  def lists = li1 | li2 | li3
  def li1: PS = """^ [*-] """.r ^^ { case _ => "    * " }
  def li2: PS = """^  [*-] """.r ^^ { case _ => "        * " }
  def li3: PS = """^   [*-] """.r ^^ { case _ => "            * " }

  def iframe: PS = "<iframe" ~> """[^>]*""".r <~ ">" <~ opt(""" *</iframe>""".r) ^^ {
    case a => {
      val url = a.replaceAll(""".*src="([^"]*)".*""", "$1")
      SState(try {
        val u = new URI(url)
        val s = u.getHost
        if(Services.isSiteTrusted(u.getHost)) ("<iframe" + a + "></iframe>")
        else ("&lt;iframe" + a + "&gt;")
      } catch {
        case _ : Throwable => ("&lt;iframe" + a + "&gt;")
      })
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

  // this is used when matching a link/name
  protected def wikiPropsRep: PS = rep(wikiProp | xstatic) ^^ {
    // LEAVE this as a SState - don't make it a LState or you will have da broblem
    case l => SState(l.map(_.s).mkString, l.flatMap(_.props).toMap, l.flatMap(_.ilinks))
  }

  // this is used for contents of a topic
  protected def wikiProps: PS =
    moreWikiProps.foldLeft(wikiPropNothing)((x,y) => x | y) | wikiProp

  protected def dotProps: PS = moreDotProps.foldLeft(dotPropNothing)((x,y) => x | y) | dotProp

  def wikiProp: PS = "{{" ~> """[^}: ]+""".r ~ """[: ]""".r ~ """[^}]*""".r <~ "}}" ^^ {
    //  def wikiProp: PS = "{{" ~> """[^}: ]+""".r ~ "[: ]".r ~ rep((arg2 | arg | arg0) <~ opt(",")) <~ "}}" ^^ {
    case name ~ _ ~ value => {
      //      val value = args.find(_._1 == "ARG0").map(_._2).getOrElse(args.mkString(","))
      // default to widgets
      if(Wikis(realm).index.containsName("widget_" + name.toLowerCase()))
        SState(
          Wikis(realm).find(WID("Admin", "widget_" + name.toLowerCase())).map(_.content).map { c =>
            //            (List(("WIDGET_ARGS", value)) ::: args).foldLeft(c)((c, a) => c.replaceAll(a._1, a._2))
            (List(("WIDGET_ARGS", value)) ).foldLeft(c)((c, a) => c.replaceAll(a._1, a._2))
          } getOrElse "")
      else if(WikiMods.index.contains(name.toLowerCase)) {
        LazyState {(current, ctx) =>
          //todo the mod to be able to add some properties in the context of the current topic
          SState(WikiMods.index(name.toLowerCase).modProp(name, value, ctx.we))
        }
      } else {
        if (name startsWith ".")
          SState("", Map(name.substring(1) -> value)) // hidden
        else
          SState(s"""<span style="font-weight:bold">{{Property $name=$value}}</span>\n\n""", Map(name -> value))
      }
    }
  }


  def dotProp: PS = """^\.""".r ~> """[.]?[^.: ][^: ]+""".r ~ """[: ]""".r ~ """[^\r\n]*""".r ^^ {
    case name ~ _ ~ value => {
      // default to widgets
      if(Wikis(realm).index.containsName("widget_" + name.toLowerCase()))
        SState(
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
  }

  private def wikiPropNothing: PS = "\\{\\{nothing[: ]".r ~> """[^}]*""".r <~ "}}" ^^ {
    case x => SState(s"""{{Nothing $x}}""", Map.empty)
  }

  private def dotPropNothing: PS = """^\.nothing """.r ~> """[^\n\r]*""".r  ^^ {
    case value => SState(s"""<small><span style="font-weight:bold;">$value</span></small><br>""", Map("name" -> value))
  }

}

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
trait WikiParserT extends WikiParserMini with CsvParser {
  import WAST._

  //======================= {{name:value}}

  // this is used when matching a link/name
  override protected def wikiPropsRep: PS = rep(wikiPropMagicName | wikiPropByName | wikiPropWhenName |
    wikiPropWhereName | wikiPropLocName | wikiPropRoles | wikiProp |
    xstatic) ^^ {
    // LEAVE this as a SState - don't make it a LState or you will have da broblem
    case l => SState(l.map(_.s).mkString, l.flatMap(_.props).toMap, l.flatMap(_.ilinks))
  }

  // this is used for contents of a topic
  override protected def wikiProps: PS =
    moreWikiProps.foldLeft(
    wikiPropISection | wikiPropMagic | wikiPropBy | wikiPropWhen | wikiPropXp | wikiPropXmap | wikiPropWhere |
    wikiPropLoc | wikiPropRoles | wikiPropAttrs | wikiPropAttr | wikiPropWidgets | wikiPropCsv | wikiPropCsv2 |
    wikiPropTable | wikiPropSection | wikiPropImg | wikiPropVideo |
    wikiPropCode | wikiPropField | wikiPropRk | wikiPropFeedRss | wikiPropTag | wikiPropExprS |
    wikiPropRed | wikiPropAlert | wikiPropLater | wikiPropHeading | wikiPropFootref | wikiPropFootnote |
    wikiPropIf | wikiPropVisible | wikiPropUserlist
    )((x,y) => x | y) | wikiProp

  override protected def dotProps: PS = moreDotProps.foldLeft(dotPropTags | dotPropName )((x,y) => x | y) | dotProp

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

  def dotPropTags: PS = """^\.t """.r ~> """[^\n\r]*""".r  ^^ {
    case value => SState("", Map("inlinetags" -> value)) // hidden
  }

  def dotPropName: PS = """^\.n """.r ~> """[^\n\r]*""".r  ^^ {
    case value => SState(s"""<small><span style="font-weight:bold;">$value</span></small><br>""", Map("name" -> value))
  }

  private def wikiPropHeading: PS = """^#+ +""".r ~ """[^\n\r]*""".r  ^^ {
    case head ~ name => {
      val u = Enc toUrl name.replaceAll(" ", "_")
      SState(s"""<a name="$u"></a>\n$head $name""") // hidden
    }
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

  private def wikiPropFootnote : PS = "{{footnote" ~> """[: ]""".r ~> """[^:}]*""".r ~ opt("[: ]".r ~> """[^}]*""".r) <~ "}}" ^^ {
    case name ~ text => s"""<span id="$name"><sup>$name</sup></span>"""+text.map(s=>
        s"""<small>$s</small>"""
    ).mkString
  }

  private def wikiPropFootref : PS = "{{footref" ~> """[: ]""".r ~> """[^:}]*""".r <~ "}}" ^^ {
    case name => s"""<a href="#$name"><sup>$name</sup></a>"""
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

  private def wikiPropUserlist: PS = "{{userlist" ~> """[: ]""".r ~> opt("[^.]*\\.".r) ~ "[^}]*".r <~ "}}" ^^ {
    case newr ~ cat => {
      // TODO can't see more than 20-
      val newRealm = if(newr.isEmpty) realm else newr.get.substring(0,newr.get.length-1)
      LazyState { (current, ctx) =>
        val res = try {
          val up = ctx.au
          val uw = up.toList.flatMap(_.myPages(newRealm, cat))
          val upp = uw.map(_.asInstanceOf[ {def wid: WID}])
          //            s"<!-- ($realm : $newRealm) ${upp.map(_.wid.wpath).mkString} ..... ${upp.map(_.wid.realm).mkString} -->" +
          "<ul>" +
            upp.sortWith(_.wid.name < _.wid.name).take(20).map(_.wid).map { wid =>
              Wikis.formatWikiLink(realm, wid, Wikis(realm).label(wid).toString, Wikis(realm).label(wid).toString, None)
            }.map(_._1).map(x => "<li>" + x + "</li>").mkString(" ") + "</ul>"
        } catch {
          case e@(_: Throwable) => {
            println(e.toString);
            "ERR Can't list userlist"
          }
        }
        SState(res)
      }
    }
  }

  // just a nice badge for RK
  private def wikiPropRk: PS = "{{" ~> ("rk" | "wiki" | "ski") ~ opt("[: ]".r ~> """[^}]*""".r) <~ "}}" ^^ {
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
          SState("""<a class="badge badge-warning" href="http://wiki.dieselapps.com">DieselApps</a>""")
        case "ski" =>
          SState("""<a class="badge badge-warning" href="http://www.effectiveskiing.com">EffectiveSkiing</a>""")
        case _ =>
          SState("""<a class="badge badge-warning" href="http://www.dieselapps.com">DieselApps</a>""")
      }
    }
  }

  private def wikiPropRed: PS = "{{" ~> "red" ~> opt("[: ]".r ~> """[^}]*""".r) <~ "}}" ^^ {
    case what => SState(s"""<span style="color:red;font-weight:bold;">${what.mkString}</span>""")
  }

  private def wikiPropLater: PS = "{{" ~> "later" ~> "[: ]".r ~> """[^ :]*""".r ~ "[: ]".r ~ """[^}]*""".r <~ "}}" ^^ {
    case id~ _ ~ url => SState(Wikis.propLater(id, url))
  }

  private def wikiPropWidgets: PS = "{{" ~> "widget[: ]".r ~> "[^: ]+".r ~ optargs <~ "}}" ^^ {
    case name ~ args => {
      val wid = WID("Admin", "widget_" + name)
      val widl = WID("Admin", "widget_" + name.toLowerCase)
      SState(
      // todo cache this
        Wikis(realm).find(wid).orElse(
          Wikis(realm).find(widl)).orElse(
            Wikis.rk.find(wid)).map(_.content).map { c =>
          args.foldLeft(c)((c, a) => c.replaceAll(a._1, a._2))
        } getOrElse "")
    }
  }

  //======================= forms

  private def wikiPropField: PS = "{{" ~> "f:" ~> "[^:]+".r ~ optargs <~ "}}" ^^ {
    case name ~ args => {
      LazyState {(current, ctx) =>
        ctx.we.foreach { we =>
          we.fields = we.fields ++ Map(name -> FieldDef(name, "", args.map(t => t).toMap))
        }
        SState("`{{{f:%s}}}`".format(name))
      }
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
  def wikiPropSection: PS = "{{" ~> opt(".") ~ """section|template|properties""".r ~ "[: ]".r ~ """[^ :}]*""".r ~ opt("[: ]".r ~ """[^}]*""".r) ~ "}}" ~ lines <~ ("{{/" ~ """section|template|properties""".r ~ "}}".r) ^^ {
    case hidden ~ stype ~ _ ~ name ~ sig ~ _ ~ lines => {
      val signature = sig.map(_._2).getOrElse("")
      //todo complete this - sections to use AST as well
      LazyState {(current, ctx) =>
        ctx.we.foreach{w=>
          //          w.collectedSections += WikiSection(ctx.we.get, stype, name, signature, lines.toString)
        }
        hidden.map(x => SState.EMPTY) getOrElse
          RState(s"`{{$stype $name:$signature}}`<br>", lines, s"<br>`{{/$stype}}` ").fold(ctx)
      }
    }
  }

//  /** {{FAU}}...{{/FAU}} */
//  def wikiPropFAU: PS = "{{" ~> "FAU[: ]".r ~ opt("[^}]+".r) ~ " *\\}\\}".r ~ lines <~ ("{{/" ~ """FAU""".r ~ " *}}".r) ^^ {
//    case stype ~ attrs ~ _ ~ lines => {
//      LazyState {(currentState, ctx) =>
//        if(api.wix) RState("", lines, "").fold(ctx)
//        else SState.EMPTY
//      }
//    }
//  }

  /** {{alert.color}}...{{/alert}} */
  def wikiPropAlert: PS = "{{" ~> "alert[: ]".r ~> """green|blue|yellow|red|black""".r ~ " *".r ~ opt("[^}]+".r) ~ " *\\}\\}".r ~ lines <~ ("{{/" ~ """alert""".r ~ " *}}".r) ^^ {
    case stype ~ _ ~ attrs ~ _ ~ lines => {
      val color = stype match {
        case "green" => "success"
        case "blue" => "info"
        case "yellow" => "warning"
        case "red" => "danger"
        case "black" => "black"
      }
      // todo someone mangles the quotes if this is just {{}}
      RState(s"""{{div class="alert alert-$color" ${attrs.mkString} }}""", lines, s"{{/div}}")
    }
  }

  def wikiPropIf: PS = "{{" ~> "if[: ]".r ~> "[^}]+".r ~ " *\\}\\}".r ~ lines <~ ("{{/" ~ """if""".r ~ " *}}".r) ^^ {
    case expr ~ _ ~ lines => {
      LazyState {(current, ctx) =>
        ctx.we.map { we =>
          //          val res = ctx.eval(, expr)
          val res = Wikis.runScript(expr, "js", ctx.we, ctx.au)
          if(res == "true") lines.fold(ctx)
          else SState.EMPTY
        } getOrElse {
          SState.EMPTY
        }
      }
    }
  }

  def wikiPropVisible: PS = "{{" ~> "visible[: ]".r ~> "[^ }:]+".r ~ " *".r ~ opt("[^}]+".r) ~ " *\\}\\}".r ~ lines <~ ("{{/" ~ """visible""".r ~ " *}}".r) ^^ {
    case expr ~ _ ~ attrs ~ _ ~ lines => {
      LazyState {(current, ctx) =>
        ctx.we.map { we =>
          var desc = attrs.map("("+ _ +")").getOrElse("<small>("+lines.fold(ctx).s.split("(?s)\\s+").size +" words)</small>")
          if(ctx.au.exists(_.hasMembershipLevel(expr)))
            SState(
              s"""{{div class="alert alert-success"}}""" + s"<b>Member-only content/discussion begins</b> ($expr)" + s"{{/div}}" +
              lines.fold(ctx).s
            )
          else if(expr != "Moderator")
            SState(s"""{{div class="alert alert-danger"}}""" + s"<b>Member-only content avilable <i>$desc</i></b>. <br>To see more on this topic, you need a membership. ($expr)" + s"{{/div}}")
          else
            SState.EMPTY
        } getOrElse {
          SState.EMPTY
        }
      }
    }
  }

  // to not parse the content, use slines instead of lines
//  def wikiPropITemplate: PS = "{{" ~> opt(".") ~ """template""".r ~ "[: ]".r ~ """[^}]*""".r ~ "}}" ~ slines ~ "{{/" ~ """template""".r ~ "}}" ^^ {
//    case hidden ~ stype ~ _ ~ name ~ _ ~ lines ~ e1 ~ e3 ~ e4 => {
//      val sname = "{{" + hidden.mkString + stype + ":" + name + "}}"
//      RState(sname, lines, e1+e3+e4)
//    }
//  }

  // to not parse the content, use slines instead of lines
  def wikiPropISection: PS = "{{`" ~> """[^}]*""".r <~ "}}" ^^ {
    case whatever => {
      SState("{{`"+ whatever+ "}}")
    }
  }

  // let it be - avoid replacing it - it's expanded in Wikis where i have the wid
  def wikiPropExprS: PS = """\{\{\$\$?""".r ~ """[^}]*""".r <~ "}}" ^^ {
    case kind ~ expr => {
      LazyState {(current, ctx) =>
        SState(ctx.eval(kind.substring(2), expr))
      }
    }
  }

  // let it be - avoid replacing it - it's expanded in Wikis where i have the wid
  def wikiPropTag: PS = "{{tag" ~ """[: ]""".r ~> """[^}]*""".r <~ "}}" ^^ {
    case name => {
      LazyState {(current, ctx) =>
        val html = Some(Wikis.hrefTag(ctx.we.get.wid, name, name))
        SState(html.get)
      }
    }
  }

  /** map nvp to html tag attrs */
  private def htmlArgs (args:List[(String,String)]) = args.foldLeft(""){(c, a) => s""" $c ${a._1}="${a._2}" """}

  def wikiPropImg: PS = "{{" ~> "img|photo".r ~ opt("""\.icon|\.small|\.medium|\.large""".r) ~ """[: ]+""".r ~ """[^} ]*""".r ~ optargs <~ "}}" ^^ {
    case skind ~ stype ~ _ ~ name ~ iargs => {
      val width = stype match {
        case Some(".icon") => "width=\"50px\""
        case Some(".small") => "width=\"200px\""
        case Some(".medium") => "width=\"400px\""
        case Some(".large") => "width=\"600px\""
        case _ => ""
      }

      val args = iargs.filter(_._1 != "caption")
      val alt = iargs.toMap.get("caption").filter(_.contains("\"") == false).map(x=>"alt=\""+x+"\"").mkString
      val caption = getCaption(iargs)
      // no alt when contains links
      skind match {
        case "img" =>   SState(s"""<img src="$name" $width $alt ${htmlArgs(args)} /><br>$caption<br>""")
        case "photo" => SState(s"""<div style="text-align:center"><a href="$name"><img src="$name" $width $alt ${htmlArgs(args)} ></a></div>$caption\n<br>""")
      }
    }
  }

  private def wikiPropVideo: PS = "{{" ~> ("video" | "slideshow") ~ """[: ]""".r ~ """[^} ]*""".r ~ optargs <~ "}}" ^^ {
    case what ~ _ ~ url ~ args => wpVideo(what, url, args)
  }

  // todo more humane name
  def wikiPropFeedRss: PS = "{{feed.rss" ~ "[: ;]".r ~> """[^ ;}]*""".r <~ "}}" ^^ {
    case xurl => {
      val id = System.currentTimeMillis().toString

      SState(s"""<div id="$id">""" + Wikis.propLater(id, "/wikie/feed?url="+Enc.toUrl(xurl)) + "</div>")
    }
  }

  def wikiPropCode: PS = "{{" ~> """code""".r ~ "[: ]".r ~ """[^:}]*""".r ~ "}}" ~ opt(CRLF1 | CRLF3 | CRLF2) ~ slines <~ "{{/code}}" ^^ {
    case stype ~ _ ~ name ~ _ ~ crlf ~ lines => {
      RState(
        "<pre><code>",
        if(name != "xml" && name != "html") lines else {
          Enc.escapeHtml(lines.s)
        },
        "</code></pre>")
    }
  }

  //=================== XP maps and lists

  private def wikiPropXmap: PS = "{{" ~> """xmap""".r ~ """[: ]""".r ~ """[^}]*""".r ~ "}}" ~ lines <~ ("{{/" ~ """xmap""".r ~ "}}") ^^ {
    case what ~ _ ~ path ~ _ ~ lines => {
      LazyState {(current, ctx) =>
        val html =
          try {
            val s = lines.fold(ctx).s
            ctx.we.map {x =>
              val values = Wikis.irunXp(what, x, path)
              values.map {value=>
                val PAT = """\$\{([^}]+)\}""".r
                PAT replaceSomeIn (s, { m =>
                  if(m.group(1) == "value") Some(value.toString)
                  else None
                })
              }.mkString
            }
          } catch {
            case ex: Throwable => Some("`{{ERROR: "+ex.toString+"}}`")
          }

        SState(html.mkString)
      }
    }
  }

  private def wikiPropXp: PS = "{{" ~> """xpl?""".r ~ """[: ]""".r ~ """[^}]*""".r <~ "}}" ^^ {
    case what ~ _ ~ path => {
      LazyState { (current, ctx) =>
        SState( s"""`{{{$what:$path}}}`""", Map())
      }
      // can't expand this during parsing as it will recursively mess up XP
      //      LazyState {(current, we) =>
      //        val html =
      //          try {
      //            we.map(x => Wikis.runXp(what, x, path))
      //          } catch {
      //            case _: Throwable => Some("!?!")
      //          }
      //
      //        SState(html.get)
      //      }
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


