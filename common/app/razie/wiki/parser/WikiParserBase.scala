/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.parser

import org.bson.types.ObjectId
import razie.wiki.{Services, Enc}
import razie.{cdebug, cout, clog}

import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.util.matching.Regex.Match
import scala.util.parsing.combinator.RegexParsers
import scala.Option.option2Iterable
import scala.collection.mutable
import razie.wiki.model._

object ParserSettings {
  /** debug the buildig of AST while pasing */
  var debugStates = false

  WikiObservers mini {
    case x:WikiConfigChanged =>
      Services.config.sitecfg("ParserSettings.debugStates").foreach { s =>
        debugStates = s.toBoolean
      }
  }

  //======================= forbidden html tags TODO it's easier to allow instead?

  //todo form|input allowed?

  final val hok = "abbr|acronym|address|a|b|blockquote|br|div|dd|dl|dt|font|h1|h2|h3|h4|h5|h6|hr|i|img|li|p|pre|q|s|small|strike|strong|span|sub|sup|" +
    "table|tbody|td|tfoot|th|thead|tr|ul|u|input|form|textarea|select|label"
  final val hnok = "applet|area|base|basefont|bdo|big|body|button|caption|center|cite|code|colgroup|col|" +
    "del|dfn|dir|fieldset|frame|frameset|head|html|iframe|ins|isindex|kbd|" +
    "legend|link|map|menu|meta|noframes|noscript|object|ol|" +
    "optgroup|option|param|samp|script|style|title|tt|var"

  final val mth1 = "Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec"
  final val mth2 = "January|February|March|April|May|June|July|August|September|October|November|December"
}

/** simple parsers */
trait ParserCommons extends RegexParsers {
  override def skipWhitespace = false

  type P = Parser[String]

  type LS2 = List[List[String]]
  type LS1 = List[String]
  type PS2 = Parser[List[List[String]]]
  type PS1 = Parser[List[String]]

  def CRLF1: P = CRLF2 <~ "RKHABIBIKU" <~ not("""[^a-zA-Z0-9-]""".r) ^^ { case ho => ho + "<br>" } // hack: eol followed by a line - DISABLED
  def CRLF2: P = ("\r\n" | "\n") // normal eol
  def CRLF3: P = CRLF2 ~ """[ \t]*""".r ~ CRLF2 ^^ { case a ~ _ ~ b => a + b } // an empty line = two eol
  def NADA: P = ""

  // static must be stopped to not include too much - that's why the last expr

  // TODO optimize this - perhaps overkill by character, eh?
  // todo this version still creates issues with [xx](yy) and boxed errors (stack)
//  def static: P = (""".""".r) ^^ { case b => b.mkString }
  def oneStatic: P = not("{{") ~> not("[[") ~> not("}}") ~> not("[http:") ~> not("""http:""".r) ~> ( """"http""" | """.""".r ) ^^ { case a => a }
  def static: P = oneStatic ~ rep(not("""[{}\[\]`\r\n]""".r) ~> oneStatic) ^^ { case a ~ b => a + b.mkString }
  // todo this version eats all sequences like http://xxx as soon as a line starts
//  def static: P = not("{{") ~> not("[[") ~> not("}}") ~> not("[http") ~> (""".""".r) ~ ("""[^{}\[\]`\r\n]""".r*) ^^ { case a ~ b => a + b.mkString }
}

/** wiki parser base definitions shared by different wiki parser */
trait WikiParserBase extends ParserCommons {
  import WAST._
  
  /** provide a realm */
  def realm:String
  
  type PS = Parser[PState]

  val moreDotProps = new ListBuffer[Parser[PState]]()
  val moreWikiProps = new ListBuffer[Parser[PState]]()
  val moreBlocks = new ListBuffer[Parser[PState]]()

  // todo half combinators to allow user modules to define new rules
  def withDotProp  (p:Parser[PState]) : WikiParserBase = {moreDotProps append p; this }
  def withWikiProp (p:Parser[PState]) : WikiParserBase = {moreWikiProps append p; this}
  def withBlocks   (p:Parser[PState]) : WikiParserBase = {moreBlocks append p; this}

  //=========================== forward defs - these are used in other mini-parsers but can't break them out of main

  /** a line of wiki (to CR/LF) */
  def line: PS

  /** an optional line */
  def optline: PS

  /** a sequence of lines */
  def lines: PS

  //=========================== basic parseing rules

  def xCRLF1: PS = CRLF1 ^^ { case x => x }
  def xCRLF2: PS = CRLF2 ^^ { case x => x }
  def xCRLF3: PS = CRLF3 ^^ { case x => x }
  def xNADA: PS = NADA ^^ { case x => x }
  def xstatic: PS = static ^^ { case x => x }
  def escaped: PS = "`" ~ opt(""".[^`]*""".r) ~ "`" ^^ { case a ~ b ~ c => a + b.mkString + c }
  def escaped1: PS = "``" ~ opt(""".*""".r) ~ "``" ^^ { case a ~ b ~ c => a + b.mkString + c }
  def escaped2: PS = "```" ~ opt("js"|"scala"|"xml"|"html"|"diesel") ~ opt(CRLF1 | CRLF3 | CRLF2) ~ """(?s)[^`]*""".r ~ "```" ^^ {
    case a ~ name ~ _ ~ b ~ c => {
      RState(
        "<pre><code>",
        if (name != "xml" && name != "html") b
        else {
          Enc.escapeHtml(b)
        },
        "</code></pre>")
//      a + b + c
    }
  }

  // ======================== static lines - not parsed

  def escbq: PS = "``" ^^ { case a => SState("`") }

  // knockoff has an issue with lines containing just a space but no line ending
  def lastLine: PS = ("""^[\s]+$""".r) ^^ { case a => "\n"}

  // block static, used for blocks of code or DSL etc
  def sstatic: PS = not("{{/" | "{{xx`/" | "```" | """^\./""".r ) ~> (""".""".r) ~ ("""[^{}\[\]`\r\n]""".r*) ^^ { case a ~ b => SState(a + b.mkString) }
  def sline: PS = rep(lastLine | sstatic) ^^ {
    // leave as SState for DSL parser
    case l => SState(l.map(_.s).mkString, l.flatMap(_.props).toMap, l.flatMap(_.ilinks))
  }

  def soptline: PS = opt(sline) ^^ { case o => o.map(identity).getOrElse(SState.EMPTY) }

  def slines: Parser[SState] = rep(soptline ~ (CRLF1 | CRLF3 | CRLF2)) ~ opt(sline) ^^ {
    case l ~ c =>
      // leave as SState for DSL parser
      SState(
        l.map(t => t._1.s + t._2.s).mkString + c.map(_.s).getOrElse(""),
        l.flatMap(_._1.props).toMap ++ c.map(_.props).getOrElse(Map()),
        l.flatMap(_._1.ilinks).toList ++ c.map(_.ilinks).getOrElse(Nil))
  }

  // ======================== args

  def ws = whiteSpace

  def ows = opt(whiteSpace)

  // todo NOT WORKING - if used in optargs generates continuous loop... simple value - i.e. no name, a default argument
  protected def arg0 = "[^ :=,}]*".r <~ not("\\s*=".r) ^^ { case v => (v, "") }
  // simple x=y
  protected def arg = "[^ :=,}]*".r ~ "=" ~ "[^},]*".r ^^ { case n ~ _ ~ v => (n, v) }
  // if contains comma, use ""
//  protected def arg2 = "[^:=,}]*".r ~ "=\"" ~ "[^\"]*".r <~ "\"" ^^ { case n ~ _ ~ v => (n, v) }
  protected def arg2 = "[^ :=,}]*".r ~ "=\"" ~ "([^\"=\\\\]*(?:\\\\.[^\"=\\\\]*)*)".r <~ "\"" ^^ { case n ~ _ ~ v => (n, v.replaceAll("\\\\\"", "\"").replaceAll("\\\\=", "=")) }

  protected def optargs : Parser[List[(String,String)]] = opt("[: ]".r ~ rep((arg2 | arg ) <~ opt(","))) ^^ {
    case Some(_ ~ l) => l
    case None => List()
  }

  // for wikiProp - something's weird
  protected def noptargs : Parser[List[(String,String)]] = "[: ]".r ~ rep((arg2 | arg) <~ opt(",")) ^^ {
    case _ ~ l => l
  }
}


