/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.parser

import java.net.URI

import razie.wiki.Services
import razie.wiki.model._
import razie.wiki.mods.WikiMods

import scala.Option.option2Iterable
import scala.util.parsing.combinator.token.Tokens

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
        ParseWLink(realm, identity, p.get.s).map(x => SState(x._1, Map(), x._2.map(x => ILink(x.wid, x.label, x.role, p.get.props, p.get.ilinks)).toList)).getOrElse(SState("")) // TODO something with the props
      } else {
        // this is a normal ilink
        ParseWLink(realm, Wikis.formatName _, name).map(x => SState(x._1, Map(), x._2.toList)).getOrElse(SState(""))
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
  }

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
    case name ~ _ ~ value => {
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


