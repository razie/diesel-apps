/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.parser

import razie.tconf.parser.{TriAstNode, StrAstNode}

import scala.util.parsing.combinator.token.Tokens

/**
  * simple parser for example and tests. derived from WikiParserBase to reduce dependencies
  */
trait SimpleSpecParser extends ParserBase with Tokens {

  def apply(input: String) = {
    parseAll(wiki, input) match {
      case Success(value, _) => value
      // don't change the format of this message
      case NoSuccess(msg, next) => StrAstNode(s"[[CANNOT PARSE]] [${next.pos.toString}] : ${msg}")
    }
  }

  /** use this to parse wiki markdown on the spot - it is meant for short strings within like a cell or something */
  def parseLine(input: String) = parseAll(line, input) getOrElse StrAstNode("[[CANNOT PARSE]]")

  //============================== wiki parsing

  def wiki: PS = lines | line | xCRLF2 | xNADA

  def line: PS = rep(escaped2 | escaped1 | escaped | link2 | wikiProps | lastLine | xstatic) ^^ {
    case l => l
  }

  def optline: PS = opt(escaped2 | dotProps | line) ^^ { case o => o.map(identity).getOrElse(StrAstNode.EMPTY) }

  private def TSNB : PS = "^THISSHALTNOTBE$" ^^ { case x => StrAstNode(x) }
  private def blocks : PS = moreBlocks.fold(TSNB)((x,y) => x | y)

  def lines: PS = rep((blocks ~ CRLF2) | (optline ~ (CRLF1 | CRLF3 | CRLF2))) ~ opt(escaped2 | dotProps | line) ^^ {
    case l ~ c =>
      l.map(t => t._1 match {
        // just optimizing to reduce the number of resulting elements
        //        case ss:SState => ss.copy(s = ss.s+t._2)
        case _ => TriAstNode("", t._1, t._2)
      }) ::: c.toList
  }

  final val wikip2a = """([^:|\]]*::)?([^:|\]]*:)?([^/:|\]]*[/:])?([^|\]]+)([ ]*[|][ ]*)?([^]]*)?"""

  // just leave MD []() links alone
  def link2: PS = """\[[^\]]*\]\([^)]+\)""".r ^^ {
    case a => a
  }

  // this is used when matching a link/name
  protected def wikiPropsRep: PS = rep(wikiProp | xstatic) ^^ {
    // LEAVE this as a SState - don't make it a LState or you will have da broblem
    case l => StrAstNode(l.map(_.s).mkString, l.flatMap(_.props).toMap, l.flatMap(_.ilinks))
  }

  // this is used for contents of a topic
  protected def wikiProps: PS = moreWikiProps.foldLeft(wikiPropNothing)((x,y) => x | y) | wikiProp

  protected def dotProps: PS = moreDotProps.foldLeft(dotPropNothing)((x,y) => x | y) | dotProp

  def wikiProp: PS = "{{" ~> """[^}: ]+""".r ~ """[: ]""".r ~ """[^}]*""".r <~ "}}" ^^ {
    case name ~ _ ~ value => {
        if (name startsWith ".")
          StrAstNode("", Map(name.substring(1) -> value)) // hidden
        else
          StrAstNode(s"""<span style="font-weight:bold">{{Property $name=$value}}</span>\n\n""", Map(name -> value))
      }
  }

  def dotProp: PS = """^\.""".r ~> """[.]?[^.: ][^: ]+""".r ~ """[: ]""".r ~ """[^\r\n]*""".r ^^ {
    case name ~ _ ~ value => {
        if (name startsWith ".")
          StrAstNode ("", Map (name.substring (1) -> value) ) // hidden
        else
          StrAstNode (s"""<span style="font-weight:bold">{{Property $name=$value}}</span>\n\n""", Map (name -> value) )
      }
  }

  private def wikiPropNothing: PS = "\\{\\{nothing[: ]".r ~> """[^}]*""".r <~ "}}" ^^ {
    case x => StrAstNode(s"""{{Nothing $x}}""", Map.empty)
  }

  private def dotPropNothing: PS = """^\.nothing """.r ~> """[^\n\r]*""".r  ^^ {
    case value => StrAstNode(s"""<small><span style="font-weight:bold;">$value</span></small><br>""", Map("name" -> value))
  }

}


