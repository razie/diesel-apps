/**
  *  ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.wiki.parser

import razie.diesel.dom.RDOM.P
import razie.diesel.dom._
import razie.diesel.expr.{AExpr2, AExprFunc, AExprIdent, BCMP1, BCMP2, BCMPConst, BCMPNot, BCMPSingle, BExpr, BExprBlock, BlockExpr, CExpr, CExprNull, Expr, ExprRange, JArrExpr, JBlockExpr, JSSExpr, LambdaFuncExpr, SCExpr}
import razie.diesel.ext._
import razie.tconf.parser.{BaseAstNode, StrAstNode}
import scala.util.parsing.combinator.RegexParsers

/** expressions parser */
trait ExprParser extends RegexParsers {

  def ws = whiteSpace

  def ows = opt(whiteSpace)

  /** a regular ident but also something in single quotes 'a@habibi.34 and - is a good ident eh' */
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

  /** qualified idents, . notation, parsed as a single string */
  def qident: Parser[String] = ident ~ rep("." ~> ident) ^^ {
    case i ~ l => (i :: l).mkString(".")
  }

  // fix this somehow - these need to be accessed as this - they should be part of a "diesel" object with callbacks
  def qlidentDiesel: Parser[List[String]] = "diesel." ~ ident ^^ {
    case d ~ i => List(d+i)
  }

  /** qualified idents, . notation, parsed as a list */
  def realmqlident: Parser[List[String]] = ident ~ rep("." ~> ident) ^^ {
    case i ~ l => i :: l
  }

  def qlident: Parser[List[String]] = qlidentDiesel | realmqlident

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

  //
  //=========================== expressions and conditions ============================
  //

//  def expr: Parser[Expr] = ppexpr | cond | pterm1
  def expr:  Parser[Expr] = ppexpr1 | pterm1
  def expr2: Parser[Expr] = ppexpr2 | pterm1
  def expr3: Parser[Expr] = ppexpr3 | pterm1

  def opsmaps: Parser[String] = "as" | "map" | "filter"
  def opsmult: Parser[String] = "*" | "/"
  def opsplus: Parser[String] = "+" | "-" | "||" | "|"

  def ppexpr1: Parser[Expr] = ppexpr2 ~ ows ~ opsmaps ~ ows ~ ppexpr1 ^^ {
    case a ~ _ ~ op ~ _ ~ e => {
      // todo how to go left associative
      if(e.isInstanceOf[AExpr2] && Array("filter", "map").contains(e.asInstanceOf[AExpr2].op)) {
        val b = e.asInstanceOf[AExpr2]
        // flip right-associative to left
        AExpr2(AExpr2(a, op, b.a), b.op, b.b)
      } else {
        AExpr2(a, op, e)
      }
    }
  } | ppexpr2

  def ppexpr2: Parser[Expr] = ppexpr3 ~ ows ~ opsplus ~ ows ~ ppexpr2 ^^ {
    case a ~ _ ~ op ~ _ ~ e => AExpr2(a, op, e)
  } | ppexpr3

  def ppexpr3: Parser[Expr] = pterm1 ~ ows ~ opsmult ~ ows ~ ppexpr3 ^^ {
    case a ~ _ ~ op ~ _ ~ e => AExpr2(a, op, e)
  } | pterm1

  // todo this can't parse properly a map b map c map d
  def ppexpr6: Parser[Expr] = pterm1 ~ rep(ows ~> ("*" | "+" | "-" | "||" | "|" | "as" | "map" | "filter") ~ ows ~ pterm1) ^^ {
    case a ~ l if l.isEmpty => a
    case a ~ l => l.foldLeft(a)((x, y) =>
      y match {
        case op ~ _ ~ p => AExpr2(x, op, p)
      }
    )
  }

  def jnull: Parser[Expr] = "null" ^^ {
    case b => new CExprNull
  }

  def pterm1: Parser[Expr] = numexpr | bcexpr | escexpr | cexpr | bcexpr | jnull | xpident | //cond |
      lambda | jsexpr2 | jsexpr1 |
      scexpr2 | scexpr1 | afunc | aidentaccess | aident | jsexpr4 |
      exregex | eblock | jarray | jobj

  def lambda: Parser[Expr] = ident ~ ows ~ "=>" ~ ows ~ (expr2 | "(" ~> expr <~ ")") ^^ {
    case id ~ _ ~ a ~ _ ~ ex => LambdaFuncExpr(id, ex)
  }

  def jsexpr1: Parser[Expr] = "js:" ~> ".*(?=[,)])".r ^^ { case li => JSSExpr(li) }
  def jsexpr2: Parser[Expr] = "js:{" ~> ".*(?=})".r <~ "}" ^^ { case li => JSSExpr(li) }
  //  def jsexpr3: Parser[Expr] = "js:{{ " ~> ".*(?=})".r <~ "}}" ^^ { case li => JSSExpr(li) }
  def scexpr1: Parser[Expr] = "sc:" ~> ".*(?=[,)])".r ^^ { case li => SCExpr(li) }
  def scexpr2: Parser[Expr] = "sc:{" ~> ".*(?=})".r <~ "}" ^^ { case li => SCExpr(li) }

  def eblock: Parser[Expr] = "(" ~ ows ~> expr <~ ows ~ ")" ^^ { case ex => BlockExpr(ex) }

  // inline js expr: //1+2//
  def jsexpr4: Parser[Expr] = "//" ~> ".*(?=//)".r <~ "//" ^^ { case li => JSSExpr(li) }

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
    case b => new CExpr(b, WTypes.wt.BOOLEAN)
  }

//  def jother: Parser[String] = "[^{}\\[\\],]+".r ^^ { case ex => ex }
  def jother: Parser[Expr] = expr ^^ { case ex => ex }

  // a number
  def numexpr: Parser[Expr] = (afloat | aint ) ^^ { case i => new CExpr(i, WTypes.wt.NUMBER) }

  // string const with escaped chars
  def cexpr: Parser[Expr] = "\"" ~> """(\\.|[^\"])*""".r <~ "\"" ^^ {
    e => new CExpr(e.replaceAll("\\\\(.)", "$1"), WTypes.wt.STRING)
  }

  // escaped multiline string const with escaped chars
  // we're removing the first \n
  def escexpr: Parser[Expr] = "\"\"\"" ~ opt("\n") ~> """(?s)((?!\"\"\").)*""".r <~ "\"\"\"" ^^ {
    e => new CExpr(e.replaceAll("\\\\(.)", "$1"), WTypes.wt.STRING)
  }

  def bcexpr: Parser[Expr] = ("true" | "false") ^^ {
    b => new CExpr(b, WTypes.wt.BOOLEAN)
  }

  // XP identifier (either json or xml)
  def xpident: Parser[Expr] = "xp:" ~> xpath ^^ { case i => new XPathIdent(i) }

  // regular expression, JS style
  def exregex: Parser[Expr] =
    """/[^/]*/""".r ^^ { case x => new CExpr(x, WTypes.wt.REGEX) }

  //==================================== ACCESSORS

  // qualified identifier
  def aident: Parser[AExprIdent] = qlident ^^ { case i => new AExprIdent(i.head, i.tail.map(P("", _))) }

  // simple qident or complex one
  def aidentExpr: Parser[AExprIdent] = aidentaccess | aident

  // full accessor to value: a.b[4].c.r["field1"]["subfield2"][4].g
  // note this kicks in at the first use of [] and continues... so that aident above catches all other

  def aidentaccess: Parser[AExprIdent] = qlident ~ (sqbraccess | sqbraccessRange | accessorNum) ~ accessors ^^ {
    case i ~ sa ~ a => new AExprIdent(i.head, i.tail.map(P("", _)) ::: sa :: a)
  }

  def accessors: Parser[List[RDOM.P]] = rep(sqbraccess | sqbraccessRange | accessorIdent | accessorNum)

  private def accessorIdent: Parser[RDOM.P] = "." ~> ident ^^ {case id => P("", id, WTypes.wt.STRING)}

  private def accessorNum: Parser[RDOM.P] = "." ~> "[0-9]+".r ^^ {case id => P("", id, WTypes.wt.NUMBER)}

  private def sqbraccess: Parser[RDOM.P] = "\\[".r ~> ows ~> expr <~ ows <~ "]" ^^ {
    case e => P("", "").copy(expr=Some(e))
  }
  // for now the range is only numeric
  private def sqbraccessRange: Parser[RDOM.P] = "\\[".r ~> ows ~> numexpr ~ ows ~ ".." ~ ows ~ opt(numexpr) <~ ows <~ "]" ^^ {
    case e1 ~ _ ~ _ ~ _ ~ e2 => P("", "", WTypes.wt.RANGE).copy(
      expr = Some(ExprRange(e1, e2))
    )
  }


  //==================================== F U N C T I O N S

  def afunc: Parser[Expr] = qident ~ attrs ^^ { case i ~ a => AExprFunc(i, a) }

  def optKinds: Parser[Option[String]] = opt(ows ~> "[" ~> ows ~> repsep(ident, ",") <~ "]") ^^ {
    case Some(tParm) => Some(tParm.mkString)
    case None => None
  }

  /**
    * expr assignment, left side can be a[5].name
    */
  def pasattr: Parser[PAS] = " *".r ~> (aidentaccess | aident) ~ opt(" *= *".r ~> expr) ^^ {
    case ident ~ e => {
      e match {
        case Some(ex) => PAS(ident, ex)
        case None => PAS(ident, ident) // compatible for a being a=a
      }
    }
  }

  /**
    * simple ident = expr assignemtn when calling
    */
  def pcallattrs: Parser[List[RDOM.P]] = " *\\(".r ~> ows ~> repsep(pcallattr, ows ~ "," ~ ows) <~ ows <~ ")"
  def pcallattr: Parser[P] = " *".r ~> qident ~ opt(" *= *".r ~> expr) ^^ {
    case ident ~ ex => {
      P(ident, "", ex.map(_.getType).getOrElse(WTypes.wt.EMPTY), ex)
    }
  }

  def pasattrs: Parser[List[PAS]] = " *\\(".r ~> ows ~> repsep(pasattr, ows ~ "," ~ ows) <~ ows <~ ")"

  /**
    * :<>type[kind]*
    * <> means it's a ref, not ownership
    * * means it's a list
    */
  def optType: Parser[WType] = opt(" *: *".r ~> opt("<>") ~ ident ~ optKinds ~ opt(" *\\* *".r)) ^^ {
    case Some(ref ~ tt ~ k ~ None) => WType(tt, "", k).withRef(ref.isDefined)
    case Some(ref ~ tt ~ k ~ Some(_)) => WType(WTypes.ARRAY, "", Some(tt)).withRef(ref.isDefined)
    case None => WTypes.wt.EMPTY
  }

  /**
    * name:<>type[kind]*~=default
    * <> means it's a ref, not ownership
    * * means it's a list
    */
  def pattr: Parser[RDOM.P] = " *".r ~> qident ~ optType ~ opt(" *~?= *".r ~> expr) ^^ {

    case name ~ t ~ e => {
      val (dflt, ex) = e match {
        //        case Some(CExpr(ee, "String")) => (ee, None)
        // todo good optimization but I no longer know if some parm is erased like (a="a", a="").
        case Some(expr) => ("", Some(expr))
        case None => ("", None)
      }
      t match {
        // k - kind is [String] etc
        case WTypes.wt.EMPTY => // infer type from expr
          P(name, dflt, ex.map(_.getType).getOrElse(WTypes.wt.EMPTY), ex)
        case tt => // ref or no archetype
          P(name, dflt, tt, ex)
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
  def like: Parser[BExpr] = ibex("~=" | "matches")
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

  def parseIdent (input: String):Option[AExprIdent] = {
    parseAll(aidentExpr, input) match {
      case Success(value, _) => Some(value)
      case NoSuccess(msg, next) => None
    }
  }
}

/** assignment - needed because the left side is more than just a val */
case class PAS (left:AExprIdent, right:Expr) extends CanHtml {
  override def toHtml = left.toHtml + "=" + right.toHtml
  override def toString = left.toString + "=" + right.toString
}
