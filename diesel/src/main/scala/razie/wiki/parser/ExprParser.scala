/**
  *  ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.wiki.parser

import razie.diesel.dom._
import razie.diesel.ext.{BFlowExpr, FlowExpr, MsgExpr, SeqExpr}
import razie.tconf.parser.SState

import scala.util.parsing.combinator.RegexParsers

/** expressions parser */
trait ExprParser extends RegexParsers {

  type P = Parser[String]

  def ws = whiteSpace

  def ows = opt(whiteSpace)

  /** a regular ident but also '...' */
  def ident: P = """[a-zA-Z_][\w]*""".r | """'[\w@. -]+'""".r ^^ {
    case s =>
      if(s.startsWith("'") && s.endsWith("'"))
        s.substring(1, s.length-1)
      else
        s
  }

  def qident: P = ident ~ rep("." ~> ident) ^^ {
    case i ~ l => (i :: l).mkString(".")
  }

  def boolConst: P = "true" | "false" ^^ {
    case b => b
  }

  def xpath: P = ident ~ rep("[/@]+".r ~ ident) ^^ {
    case i ~ l => (i :: l.map{x=>x._1+x._2}).mkString("")
  }

  def any: P = """.*""".r

  //todo full expr with +-/* and XP
  def value: P = boolConst | qident | afloat | aint | str

  def aint: P = """-?\d+""".r
  def afloat: P = """-?\d+[.]\d+""".r

  // todo commented - if " not included in string, evaluation has trouble - see expr(s)
  // todo see stripq and remove it everywhere when quotes die and proper type inference is used
  def str: P = "\"" ~> """[^"]*""".r <~ "\""

  //  def str: P = """"[^"]*"""".r

  //------------ expressions and conditions

  def expr: Parser[Expr] = ppexpr | pterm1

  def ppexpr: Parser[Expr] = pterm1 ~ rep(ows ~> ("*" | "+" | "-" | "||" | "|") ~ ows ~ pterm1) ^^ {
    case a ~ l if l.isEmpty => a
    case a ~ l => l.foldLeft(a)((a, b) =>
      b match {
        case op ~ _ ~ p => AExpr2(a, op, p)
      }
    )
  }

  def pterm1: Parser[Expr] = numexpr | bcexpr | cexpr | xident | jsexpr1 | jsexpr2 | aident | jss | exregex | eblock | js

  def eblock: Parser[Expr] = "(" ~ ows ~> expr <~ ows ~ ")" ^^ { case ex => BlockExpr(ex) }

  def jss: Parser[Expr] = "//" ~> ".*(?=//)".r <~ "//" ^^ { case li => JSSExpr(li) }

  def js: Parser[Expr] = "{" ~ ows ~> repsep(jnvp <~ ows, ",") <~ ows ~ "}" ^^ { case li => JBlockExpr(li.mkString(",")) }
//  def js: Parser[Expr] = "{" ~ ows ~> jexpr <~ ows ~ "}" ^^ { case ex => JBlockExpr(ex) }

  def jobj: Parser[String] = "{" ~ ows ~> repsep(jnvp <~ ows, ",") <~ ows ~ "}" ^^ { case li => "{ " + li.mkString(",") + " }" } // just because type of parser is String not Expr

  def jnvp: Parser[String] = ows ~> ident ~ " *: *".r ~ jexpr ^^ { case name ~ _ ~ ex =>  name +":" + ex.toString }

  def jarray: Parser[String] = "[" ~ ows ~> repsep(jexpr <~ ows, ",") <~ ows ~ "]" ^^ { case li => "[ " + li.mkString(",") + " ]" }

  def jexpr: Parser[String] = jobj | jarray | jother ^^ { case ex => ex.toString }

  def jother: Parser[String] = "[^{}\\[\\],]+".r ^^ { case ex => ex }

  // a number
  def numexpr: Parser[Expr] = (afloat | aint ) ^^ { case i => new CExpr(i, WTypes.NUMBER) }

  // string const with escaped chars
  def cexpr: Parser[Expr] = "\"" ~> """(\\.|[^\"])*""".r <~ "\"" ^^ {
    case e => new CExpr(e.replaceAll("\\\\(.)", "$1"), WTypes.STRING)
  }

  def bcexpr: Parser[Expr] = ("true" | "false") ^^ {
    case b => new CExpr(b, WTypes.BOOLEAN)
  }

  // qualified identifier
  def aident: Parser[Expr] = qident ^^ { case i => new AExprIdent(i) }

  // XP identifier (either json or xml)
  def xident: Parser[Expr] = "xp:" ~> xpath ^^ { case i => new XPathIdent(i) }

  def jsexpr1: Parser[Expr] = "js:" ~> ".*(?=[,)])".r ^^ { case li => JSSExpr(li) }
  def jsexpr2: Parser[Expr] = "js:{" ~> ".*(?=})".r <~ "}" ^^ { case li => JSSExpr(li) }

  // regular expression, JS style
  def exregex: Parser[Expr] =
    """/[^/]*/""".r ^^ { case x => new CExpr(x, WTypes.REGEX) }

  //------------ conditions

  def cond: Parser[BExpr] = boolexpr

  def boolexpr: Parser[BExpr] = bterm1 | bterm1 ~ ("||" | "or") ~ bterm1 ^^ { case a ~ s ~ b => bcmp(a, s.trim, b) }

  def bterm1: Parser[BExpr] = bfactor1 | bfactor1 ~ ("&&" | "and") ~ bfactor1 ^^ { case a ~ s ~ b => bcmp(a, s.trim, b) }

  def bConst: Parser[BExpr] = ("true" | "false") ^^ {
    case b => BCMPConst(b)
  }

  def bfactor1: Parser[BExpr] = eq | neq | lte | gte | lt | gt

  def like: Parser[BExpr] = expr ~ "~=" ~ expr ^^ { case a ~ s ~ b => cmp(a, s.trim, b) }

  def df: Parser[BExpr] = expr ~ "?=" ~ expr ^^ { case a ~ s ~ b => cmp(a, s.trim, b) }

  def eq: Parser[BExpr] = expr ~ ("==" | "is") ~ expr ^^ { case a ~ s ~ b => cmp(a, s.trim, b) }

  def neq: Parser[BExpr] = expr ~ ("!=" | "not") ~ expr ^^ { case a ~ s ~ b => cmp(a, s.trim, b) }

  def lte: Parser[BExpr] = expr ~ "<=" ~ expr ^^ { case a ~ s ~ b => cmp(a, s.trim, b) }

  def gte: Parser[BExpr] = expr ~ ">=" ~ expr ^^ { case a ~ s ~ b => cmp(a, s.trim, b) }

  def lt: Parser[BExpr] = expr ~ "<" ~ expr ^^ { case a ~ s ~ b => cmp(a, s.trim, b) }

  def gt: Parser[BExpr] = expr ~ ">" ~ expr ^^ { case a ~ s ~ b => cmp(a, s.trim, b) }

  def bcmp(a: BExpr, s: String, b: BExpr) = new BCMP1(a, s, b)

  def cmp(a: Expr, s: String, b: Expr) = new BCMP2(a, s, b)

  // ---------------------- flow expressions

  def flowexpr: Parser[FlowExpr] = seqexpr

  def seqexpr: Parser[FlowExpr] = parexpr ~ rep(ows ~> ("+" | "-") ~ ows ~ parexpr) ^^ {
    case a ~ l =>
      SeqExpr("+", a :: l.collect {
        case op ~ _ ~ p => p
      })
  }

  def parexpr: Parser[FlowExpr] = parterm1 ~ rep(ows ~> ("|" | "||") ~ ows ~ parterm1) ^^ {
    case a ~ l =>
      SeqExpr("|", a :: l.collect {
        case op ~ _ ~ p => p
      })
  }

  def parterm1: Parser[FlowExpr] = parblock | msgterm1

  def parblock: Parser[FlowExpr] = "(" ~ ows ~> seqexpr <~ ows ~ ")" ^^ {
    case ex => BFlowExpr(ex)
  }

  def msgterm1: Parser[FlowExpr] = qident ^^ { case i => new MsgExpr(i) }

}

/** A simple parser for our simple specs
  *
  * DomParser is the actual Diesel/Dom parser.
  * We extend from it to include its functionality and then we add its parsing rules with withBlocks()
  */
class SimpleExprParser extends ExprParser {

  def parseExpr (input: String):Option[Expr] = {
    parseAll(expr, input) match {
      case Success(value, _) => Some(value)
      case NoSuccess(msg, next) => None
    }
  }
}

