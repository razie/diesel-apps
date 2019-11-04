/*   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.expr

import razie.diesel.dom.RDOM.P
import razie.diesel.dom.{RDOM, WType, WTypes, XPathIdent}
import razie.diesel.engine.nodes.PAS
import scala.util.parsing.combinator.RegexParsers

/**
  * expressions parser. this is a trait you can mix in your other DSL parsers,
  * see SimpleExprParser for a concrete implementation
  *
  * See http://specs.razie.com/wiki/Story:expr_story for possible expressions and examples
  */
trait ExprParser extends RegexParsers {

  // mandatory whiteSpace
  def ws = whiteSpace

  // optional whiteSpace
  def ows = opt(whiteSpace)

  //
  //======================= MAIN: operator expressions and conditions ========================
  //

  /** main entry point for an expression */
  def expr:  Parser[Expr] = exprAS | pterm1

  // a reduced expr, from boolean down, useful for lambdas for conditions
  def expr2: Parser[Expr] = exprOR | pterm1

  private def opsAS: Parser[String] = "as"
  private def opsMAP: Parser[String] = "map" | "flatMap" | "flatten" | "filter"
  private def opsOR: Parser[String] = "or" | "xor"
  private def opsAND: Parser[String] = "and"
  private def opsCMP: Parser[String] = ">" | "<" | ">=" | "<=" | "==" | "!=" | "~=" | "?=" | "is" | "not" | "contains"
  private def opsPLUS: Parser[String] = "+" | "-" | "||" | "|"
  private def opsMULT: Parser[String] = "*" | "/"

  // "1" as number
  def exprAS: Parser[Expr] = exprMAP ~ opt(ows ~> opsAS ~ ows ~ pterm1) ^^ {
    case a ~ None => a
    case a ~ Some(op ~ _ ~ p) => AExpr2(a, op, p)
  }

  // x map (x => x+1)
  def exprMAP: Parser[Expr] = exprPLUS ~ rep(ows ~> opsMAP ~ ows ~ exprOR) ^^ {
    case a ~ l => foldAssocAexpr2(a, l, AExpr2)
  }

  // x > y
  def exprOR: Parser[Expr] = exprAND ~ rep(ows ~> opsOR ~ ows ~ exprAND) ^^ {
    case a ~ l => foldAssocAexpr2(a, l, ebcmp)
  }

  // x > y
  def exprAND: Parser[Expr] = exprCMP ~ rep(ows ~> opsAND ~ ows ~ exprCMP) ^^ {
    case a ~ l => foldAssocAexpr2(a, l, ebcmp)
  }

  // x > y
  def exprCMP: Parser[Expr] = exprPLUS ~ opt(ows ~> opsCMP ~ ows ~ exprPLUS) ^^ {
    case a ~ None => a
    case a ~ Some(op ~ _ ~ b) => cmp(a, op, b)
  }

  // x + y
  def exprPLUS: Parser[Expr] = exprMULT ~ rep(ows ~> opsPLUS ~ ows ~ exprMULT) ^^ {
    case a ~ l => foldAssocAexpr2(a, l, AExpr2)
  }

  // x * y
  def exprMULT: Parser[Expr] = pterm1 ~ rep(ows ~> opsMULT ~ ows ~ pterm1) ^^ {
    case a ~ l => foldAssocAexpr2(a, l, AExpr2)
  }

  // foldLeft associative expressions
  private def foldAssocAexpr2(a:Expr, l:List[String ~ Option[String] ~ Expr], f:(Expr, String, Expr) => Expr) = {
    l.foldLeft(a)((x, y) =>
      y match {
        case op ~ _ ~ p => f(x, op, p)
      }
    )
  }

  //
  //================== main expression rules
  //

  // a term in an expression
  def pterm1: Parser[Expr] =
    numConst | boolConst | multilineStrConst | strConst | jnull |
    xpident |
    lambda | jsexpr2 | jsexpr1 |
    scexpr2 | scexpr1 |
    afunc | aidentaccess | aident | jsexpr4 |
    exregex | eblock | jarray | jobj

  //
  //============================== idents
  //

  /** a regular ident but also something in single quotes 'a@habibi.34 and - is a good ident eh' */
  def ident: Parser[String] = """[a-zA-Z_][\w]*""".r | """'[\w@. -]+'""".r ^^ {
    case s =>
      if(s.startsWith("'") && s.endsWith("'"))
        s.substring(1, s.length-1)
      else
        s
  }

  /** allow JSON ids with double quotes, single quotes or no quotes */
  def jsonIdent: Parser[String] = """[a-zA-Z_][\w]*""".r | """'[\w@. -]+'""".r | """"[\w@. -]+"""".r ^^ {
    case s => unquote(s)
  }

  /** qualified idents, . notation, parsed as a single string */
  def qident: Parser[String] = ident ~ rep("." ~> ident) ^^ {
    case i ~ l => (i :: l).mkString(".")
  }

  def qlident: Parser[List[String]] = qlidentDiesel | realmqlident

  // fix this somehow - these need to be accessed as this - they should be part of a "diesel" object with callbacks
  def qlidentDiesel: Parser[List[String]] = "diesel." ~ ident ^^ {
    case d ~ i => List(d+i)
  }

  /** qualified idents, . notation, parsed as a list */
  def realmqlident: Parser[List[String]] = ident ~ rep("." ~> ident) ^^ {
    case i ~ l => i :: l
  }

  def xpath: Parser[String] = ident ~ rep("[/@]+".r ~ ident) ^^ {
    case i ~ l => (i :: l.map{x=>x._1+x._2}).mkString("")
  }

  //
  //==================== lambdas
  //

  // x => x + 4
  def lambda: Parser[Expr] = ident ~ ows ~ "=>" ~ ows ~ (expr2 | "(" ~> expr <~ ")") ^^ {
    case id ~ _ ~ a ~ _ ~ ex => LambdaFuncExpr(id, ex)
  }

  //
  //================================== constants - CExpr
  //

  def boolConst: Parser[Expr] = ("true" | "false") ^^ {
    b => new CExpr(b, WTypes.wt.BOOLEAN)
  }

  // a number
  def numConst: Parser[Expr]   = (afloat | aint ) ^^ { case i => new CExpr(i, WTypes.wt.NUMBER) }
  def aint:     Parser[String] = """-?\d+""".r
  def afloat:   Parser[String] = """-?\d+[.]\d+""".r

  // string const with escaped chars
  def strConst: Parser[Expr] = "\"" ~> """(\\.|[^\"])*""".r <~ "\"" ^^ {
    e => new CExpr(e.replaceAll("\\\\(.)", "$1"), WTypes.wt.STRING)
  }

  // escaped multiline string const with escaped chars
  // we're removing the first \n
  def multilineStrConst: Parser[Expr] = "\"\"\"" ~ opt("\n") ~> """(?s)((?!\"\"\").)*""".r <~ "\"\"\"" ^^ {
    e => new CExpr(e.replaceAll("\\\\(.)", "$1"), WTypes.wt.STRING)
  }

  // XP identifier (either json or xml)
  def xpident: Parser[Expr] = "xp:" ~> xpath ^^ { case i => new XPathIdent(i) }

  // regular expression, JS style
  def exregex: Parser[Expr] = """/[^/]*/""".r ^^ { case x => new CExpr(x, WTypes.wt.REGEX) }


  //
  //==================================== ACCESSORS
  //

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
  private def sqbraccessRange: Parser[RDOM.P] = "\\[".r ~> ows ~> numConst ~ ows ~ ".." ~ ows ~ opt(numConst) <~ ows <~ "]" ^^ {
    case e1 ~ _ ~ _ ~ _ ~ e2 => P("", "", WTypes.wt.RANGE).copy(
      expr = Some(ExprRange(e1, e2))
    )
  }


  //
  //==================================== F U N C T I O N S
  //

  // calling a function, this is not defining it, so no type annotations etc
  // named parameters need to be mentioned, unless it's just one
  def afunc: Parser[Expr] = qident ~ attrs ^^ { case i ~ a => AExprFunc(i, a) }

  /**
    * simple ident = expr assignemtn when calling
    */
  def pcallattrs: Parser[List[RDOM.P]] = " *\\(".r ~> ows ~> repsep(pcallattr, ows ~ "," ~ ows) <~ ows <~ ")"

  def pcallattr: Parser[P] = " *".r ~> qident ~ opt(" *= *".r ~> expr) ^^ {
    case ident ~ ex => {
      P(ident, "", ex.map(_.getType).getOrElse(WTypes.wt.EMPTY), ex)
    }
  }

  // param assignment (x = expr, ...)
  def pasattrs: Parser[List[PAS]] = " *\\(".r ~> ows ~> repsep(pasattr, ows ~ "," ~ ows) <~ ows <~ ")"

  /**
    * parm assignment, left side can be a[5].name, useful in a $val
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
    * :<>type[kind]*
    * <> means it's a ref, not ownership
    * * means it's a list
    */
  def optType: Parser[WType] = opt(" *: *".r ~> opt("<>") ~ ident ~ optKinds ~ opt(" *\\* *".r)) ^^ {
    case Some(ref ~ tt ~ k ~ None) => WType(tt, "", k).withRef(ref.isDefined)
    case Some(ref ~ tt ~ k ~ Some(_)) => WType(WTypes.ARRAY, "", Some(tt)).withRef(ref.isDefined)
    case None => WTypes.wt.EMPTY
  }

  // A [ KIND, KIND ]
  def optKinds: Parser[Option[String]] = opt(ows ~> "[" ~> ows ~> repsep(ident, ",") <~ "]") ^^ {
    case Some(tParm) => Some(tParm.mkString)
    case None => None
  }

  /**
    * parm definition / assignment
    *
    * name:<>type[kind]*~=default
    *
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


  //
  //=================== js and JSON
  //

  def jsexpr1: Parser[Expr] = "js:" ~> ".*(?=[,)])".r ^^ { case li => JSSExpr(li) }
  def jsexpr2: Parser[Expr] = "js:{" ~> ".*(?=})".r <~ "}" ^^ { case li => JSSExpr(li) }
  //  def jsexpr3: Parser[Expr] = "js:{{ " ~> ".*(?=})".r <~ "}}" ^^ { case li => JSSExpr(li) }
  def scexpr1: Parser[Expr] = "sc:" ~> ".*(?=[,)])".r ^^ { case li => SCExpr(li) }
  def scexpr2: Parser[Expr] = "sc:{" ~> ".*(?=})".r <~ "}" ^^ { case li => SCExpr(li) }

  def eblock: Parser[Expr] = "(" ~ ows ~> expr <~ ows ~ ")" ^^ { case ex => BlockExpr(ex) }

  // inline js expr: //1+2//
  def jsexpr4: Parser[Expr] = "//" ~> ".*(?=//)".r <~ "//" ^^ { case li => JSSExpr(li) }

  // remove single or double quotes if any, from ID matched with them
  def unquote(s:String) =  {
    if (s.startsWith("'") && s.endsWith("\'") || s.startsWith("\"") && s
        .endsWith("\""))
      s.substring(1, s.length - 1)
    else
      s
  }

  def jnull: Parser[Expr] = "null" ^^ {
    case b => new CExprNull
  }

  // json object - sequence of nvp assignemnts separated with commas
  def jobj: Parser[Expr] = "{" ~ ows ~> repsep(jnvp <~ ows, ",") <~ ows ~ "}" ^^ {
    case li => JBlockExpr(li)
  }

  // one json block nvp pair
  def jnvp: Parser[(String, Expr)] = ows ~> jsonIdent ~ " *[:=] *".r ~ jexpr ^^ {
    case name ~ _ ~ ex =>  (unquote(name), ex)
  }

  // array [...] - elements are expressions
  def jarray: Parser[Expr] = "[" ~ ows ~> repsep(ows ~> jexpr <~ ows, ",") <~ ows ~ "]" ^^ {
    case li => JArrExpr(li) //CExpr("[ " + li.mkString(",") + " ]")
  }

  def jexpr: Parser[Expr] = jobj | jarray | boolConst | jother ^^ { case ex => ex } //ex.toString }

  //  def jother: Parser[String] = "[^{}\\[\\],]+".r ^^ { case ex => ex }
  def jother: Parser[Expr] = expr ^^ { case ex => ex }


  //
  //==================================== C O N D I T I O N S
  //

  def cond: Parser[BoolExpr] = orexpr

  def orexpr: Parser[BoolExpr] = bterm1 ~ rep(ows ~> ("or") ~ ows ~ bterm1 ) ^^ {
    case a ~ l => l.foldLeft(a)((a, b) =>
      b match {
        case op ~ _ ~ p => bcmp (a, op, p)
      }
    )
  }

  def bterm1: Parser[BoolExpr] = bfactor1 ~ rep(ows ~> ("and") ~ ows ~ bfactor1 ) ^^ {
    case a ~ l => l.foldLeft(a)((a, b) =>
      b match {
        case op ~ _ ~ p => bcmp (a, op, p)
      }
    )
  }

  def bfactor1: Parser[BoolExpr] = notbfactor1 | bfactor2

  def notbfactor1: Parser[BoolExpr] = ows ~> ("not" | "NOT") ~> ows ~> bfactor2 ^^ { BCMPNot }

  def bfactor2: Parser[BoolExpr] = bConst | eq | neq | lte | gte | lt | gt | like | bvalue | condBlock

  private def condBlock: Parser[BoolExpr] = ows ~> "(" ~> ows ~> cond <~ ows <~ ")" ^^ { BExprBlock }

  private def cmp(a: Expr, s: String, b: Expr) = new BCMP2(a, s, b)

  private def ibex(op: => Parser[String]) : Parser[BoolExpr] = expr ~ (ows ~> op <~ ows) ~ expr ^^ {
    case a ~ s ~ b => cmp(a, s.trim, b)
  }

  /** true or false constants */
  def bConst: Parser[BoolExpr] = ("true" | "false") ^^ { BCMPConst }

  def eq: Parser[BoolExpr]   = ibex("==" | "is")
  def neq: Parser[BoolExpr]  = ibex("!=" | "not")
  def like: Parser[BoolExpr] = ibex("~=" | "matches")
  def lte: Parser[BoolExpr]  = ibex("<=")
  def gte: Parser[BoolExpr]  = ibex(">=")
  def lt: Parser[BoolExpr]   = ibex("<")
  def gt: Parser[BoolExpr]   = ibex(">")

  // default - only used in PM, not conditions
  def df: Parser[BoolExpr]   = ibex("?=")

  /** single value expressions, where != 0 is true and != null is true */
  def bvalue : Parser[BoolExpr] = expr ^^ {
    case a => BCMPSingle(a)
  }

  private def bcmp(a: BoolExpr, s: String, b: BoolExpr) = new BCMP1(a, s, b)
  private def ebcmp(a: Expr, s: String, b: Expr) = (a,b) match {
    case (a:BoolExpr, b:BoolExpr) => new BCMP1(a, s, b)
    case (a,b) => throw new DieselExprException("ebcmp - can't combine non-logical expressions with or/and")
  }

}
