/**
  *  ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.wiki.parser

import razie.diesel.dom.RDOM.P
import razie.diesel.dom._
import razie.diesel.ext._
import razie.tconf.parser.{BaseAstNode, StrAstNode}
import scala.util.parsing.combinator.RegexParsers

/** expressions parser */
trait ExprParser extends RegexParsers {

  def ws = whiteSpace

  def ows = opt(whiteSpace)

  /** a regular ident but also '...' */
  def ident: Parser[String] = """[a-zA-Z_][\w]*""".r | """'[\w@. -]+'""".r ^^ {
    case s =>
      if(s.startsWith("'") && s.endsWith("'"))
        s.substring(1, s.length-1)
      else
        s
  }

  /** allow JSON ids with double quotes */
  def jsonIdent: Parser[String] = """[a-zA-Z_][\w]*""".r | """'[\w@. -]+'""".r | """"[\w@. -]+"""".r ^^ {
    case s => unquote(s)
  }

  def qident: Parser[String] = ident ~ rep("." ~> ident) ^^ {
    case i ~ l => (i :: l).mkString(".")
  }

  def boolConst: Parser[String] = "true" | "false" ^^ {
    case b => b
  }

  def xpath: Parser[String] = ident ~ rep("[/@]+".r ~ ident) ^^ {
    case i ~ l => (i :: l.map{x=>x._1+x._2}).mkString("")
  }

  def any: Parser[String] = """.*""".r

  //todo full expr with +-/* and XP
  def value: Parser[String] = boolConst | qident | afloat | aint | str

  def aint: Parser[String] = """-?\d+""".r
  def afloat: Parser[String] = """-?\d+[.]\d+""".r

  // todo commented - if " not included in string, evaluation has trouble - see expr(s)
  // todo see stripq and remove it everywhere when quotes die and proper type inference is used
  def str: Parser[String] = "\"" ~> """[^"]*""".r <~ "\""

  //  def str: PS = """"[^"]*"""".r

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

  def jnull: Parser[Expr] = "null" ^^ {
    case b => new CExprNull
  }

  def pterm1: Parser[Expr] = numexpr | bcexpr | escexpr | cexpr | jnull | xpident | jsexpr1 | jsexpr2 |
      scexpr1 | scexpr2 | afunc | aidentaccess | aident | jsexpr3 |
      exregex | eblock | jarray | jobj

  def eblock: Parser[Expr] = "(" ~ ows ~> expr <~ ows ~ ")" ^^ { case ex => BlockExpr(ex) }

  // inline js expr: //1+2//
  def jsexpr3: Parser[Expr] = "//" ~> ".*(?=//)".r <~ "//" ^^ { case li => JSSExpr(li) }

  // json object
  def jobj: Parser[Expr] = "{" ~ ows ~> repsep(jnvp <~ ows, ",") <~ ows ~ "}" ^^ {
    case li => JBlockExpr(li)
  }

  def unquote(s:String) =  {
    if (s.startsWith("'") && s.endsWith("\'") || s.startsWith("\"") && s
          .endsWith("\""))
      s.substring(1, s.length - 1)
    else
      s
  }

  def jnvp: Parser[(String, Expr)] = ows ~> jsonIdent ~ " *[:=] *".r ~ jexpr ^^ {
    case name ~ _ ~ ex =>  (unquote(name), ex)
  }

  def jarray: Parser[Expr] = "[" ~ ows ~> repsep(ows ~> jexpr <~ ows, ",") <~ ows ~ "]" ^^ {
    case li => JArrExpr(li) //CExpr("[ " + li.mkString(",") + " ]")
  }

  def jexpr: Parser[Expr] = jobj | jarray | jbool | jother ^^ { case ex => ex } //ex.toString }

  def jbool: Parser[Expr] = ("true" | "false") ^^ {
    case b => new CExpr(b, WTypes.BOOLEAN)
  }

//  def jother: Parser[String] = "[^{}\\[\\],]+".r ^^ { case ex => ex }
  def jother: Parser[Expr] = expr ^^ { case ex => ex }

  // a number
  def numexpr: Parser[Expr] = (afloat | aint ) ^^ { case i => new CExpr(i, WTypes.NUMBER) }

  // string const with escaped chars
  def cexpr: Parser[Expr] = "\"" ~> """(\\.|[^\"])*""".r <~ "\"" ^^ {
    case e => new CExpr(e.replaceAll("\\\\(.)", "$1"), WTypes.STRING)
  }

  // escaped multiline string const with escaped chars
  def escexpr: Parser[Expr] = "\"\"\"" ~> """(?s)((?!\"\"\").)*""".r <~ "\"\"\"" ^^ {
    case e => new CExpr(e.replaceAll("\\\\(.)", "$1"), WTypes.STRING)
  }

  def bcexpr: Parser[Expr] = ("true" | "false") ^^ {
    case b => new CExpr(b, WTypes.BOOLEAN)
  }

  // XP identifier (either json or xml)
  def xpident: Parser[Expr] = "xp:" ~> xpath ^^ { case i => new XPathIdent(i) }

  def jsexpr1: Parser[Expr] = "js:" ~> ".*(?=[,)])".r ^^ { case li => JSSExpr(li) }
  def jsexpr2: Parser[Expr] = "js:{" ~> ".*(?=})".r <~ "}" ^^ { case li => JSSExpr(li) }

  def scexpr1: Parser[Expr] = "sc:" ~> ".*(?=[,)])".r ^^ { case li => SCExpr(li) }
  def scexpr2: Parser[Expr] = "sc:{" ~> ".*(?=})".r <~ "}" ^^ { case li => SCExpr(li) }

  // regular expression, JS style
  def exregex: Parser[Expr] =
    """/[^/]*/""".r ^^ { case x => new CExpr(x, WTypes.REGEX) }

  //==================================== ACCESSORS

  // qualified identifier
  def aident: Parser[AExprIdent] = qident ^^ { case i => new AExprIdent(i) }

  // full accessor to value: a.b[4].c.r["field1"]["subfield2"][4].g
  // note this kicks in at the first use of [] and continues... so that aident above catches all other
  def aidentaccess: Parser[AExprIdent] = qident ~ (sqbraccess | sqbraccessRange) ~ accessors ^^ {
    case i ~ sa ~ a => new AExprIdent(i, sa :: a)
  }

  def accessors: Parser[List[RDOM.P]] = rep(sqbraccess | sqbraccessRange | accessorIdent)

  private def accessorIdent: Parser[RDOM.P] = "." ~> ident ^^ {case id => P("", id, WTypes.STRING)}
  private def sqbraccess: Parser[RDOM.P] = "\\[".r ~> ows ~> expr <~ ows <~ "]" ^^ {
    case e => P("", "").copy(expr=Some(e))
  }
  // for now the range is only numeric
  private def sqbraccessRange: Parser[RDOM.P] = "\\[".r ~> ows ~> numexpr ~ ows ~ ".." ~ ows ~ opt(numexpr) <~ ows <~ "]" ^^ {
    case e1 ~ _ ~ _ ~ _ ~ e2 => P("", "", WTypes.RANGE).copy(
      expr = Some(ExprRange(e1, e2))
    )
  }


  //==================================== F U N C T I O N S

  def afunc: Parser[Expr] = qident ~ attrs ^^ { case i ~ a => new AExprFunc(i, a) }

  def optKinds: Parser[BaseAstNode] = opt(ows ~> "[" ~> ows ~> repsep(ident, ",") <~ "]") ^^ {
    case Some(tParm) => tParm.mkString
    case None => ""
  }

  /**
    * name:<>type[kind]*~=default
    * <> means it's a ref, not ownership
    * * means it's a list
    */
  def pasattr: Parser[PAS] = " *".r ~> (aidentaccess | aident) ~ opt(" *= *".r ~> expr) ^^ {
    case ident ~ e => {
      e match {
        case Some(ex) => PAS(ident, ex)
        case None => PAS(ident, ident) // compatible for a being a=a
      }
    }
  }

  def pasattrs: Parser[List[PAS]] = " *\\(".r ~> ows ~> repsep(pasattr, ows ~ "," ~ ows) <~ ows <~ ")"

  /**
    * name:<>type[kind]*~=default
    * <> means it's a ref, not ownership
    * * means it's a list
    */
  def pattr: Parser[RDOM.P] = " *".r ~> qident ~ opt(" *: *".r ~> opt("<> *".r) ~ ident ~ optKinds) ~
    opt(" *\\* *".r) ~ opt(" *~?= *".r ~> expr) ^^ {

    case name ~ t ~ multi ~ e => {
      val (dflt, ex) = e match {
        //        case Some(CExpr(ee, "String")) => (ee, None)
        // todo good optimization but I no longer know if some parm is erased like (a="a", a="").
        case Some(expr) => ("", Some(expr))
        case None => ("", None)
      }
      t match {
        // k - kind is [String] etc
        case Some(ref ~ tt ~ k) => // ref or no archetype
          P(name, dflt, tt + k.s, ref.mkString, multi.mkString, ex)
        case None => // infer type from expr
          P(name, dflt, ex.map(_.getType).getOrElse(""), "", multi.mkString, ex)
      }
    }
  }

  /**
    * optional attributes
    */
  def optAttrs: Parser[List[RDOM.P]] = opt(attrs) ^^ {
    case Some(a) => a
    case None => List.empty
  }

  /**
    * optional attributes
    */
  def attrs: Parser[List[RDOM.P]] = " *\\(".r ~> ows ~> repsep(pattr, ows ~ "," ~ ows) <~ ows <~ ")"


  //==================================== C O N D I T I O N S


  def cond: Parser[BExpr] = orexpr

  def orexpr: Parser[BExpr] = bterm1 ~ rep(ows ~> ("or") ~ ows ~ bterm1 ) ^^ {
    case a ~ l => l.foldLeft(a)((a, b) =>
      b match {
        case op ~ _ ~ p => bcmp (a, op, p)
      }
    )
  }

  def bterm1: Parser[BExpr] = bfactor1 ~ rep(ows ~> ("and") ~ ows ~ bfactor1 ) ^^ {
    case a ~ l => l.foldLeft(a)((a, b) =>
      b match {
        case op ~ _ ~ p => bcmp (a, op, p)
      }
    )
  }

  def bfactor1: Parser[BExpr] = notbfactor1 | bfactor2

  def notbfactor1: Parser[BExpr] = ows ~> ("not" | "NOT") ~> ows ~> bfactor2 ^^ { BCMPNot }

  def bfactor2: Parser[BExpr] = bConst | eq | neq | lte | gte | lt | gt | like | bvalue | condBlock

  private def condBlock: Parser[BExpr] = ows ~> "(" ~> ows ~> cond <~ ows <~ ")" ^^ { BExprBlock }

  private def cmp(a: Expr, s: String, b: Expr) = new BCMP2(a, s, b)

  private def ibex(op: => Parser[String]) : Parser[BExpr] = expr ~ (ows ~> op <~ ows) ~ expr ^^ {
    case a ~ s ~ b => cmp(a, s.trim, b)
  }

  def bConst: Parser[BExpr] = ("true" | "false") ^^ { BCMPConst }

  def eq: Parser[BExpr]   = ibex("==" | "is")
  def neq: Parser[BExpr]  = ibex("!=" | "not")
  def like: Parser[BExpr] = ibex("~=")
  def lte: Parser[BExpr]  = ibex("<=")
  def gte: Parser[BExpr]  = ibex(">=")
  def lt: Parser[BExpr]   = ibex("<")
  def gt: Parser[BExpr]   = ibex(">")

  // default - only used in PM, not conditions
  def df: Parser[BExpr]   = ibex("?=")

  def bvalue : Parser[BExpr] = expr ^^ {
    case a => BCMPSingle(a)
  }

  private def bcmp(a: BExpr, s: String, b: BExpr) = new BCMP1(a, s, b)


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

/** assignment */
case class PAS (left:AExprIdent, right:Expr) extends CanHtml {
  override def toHtml = left.toHtml + "=" + right.toHtml
  override def toString = left.toString + "=" + right.toString
}
