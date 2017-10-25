/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.wiki.parser

import mod.diesel.model.exec.EESnakk
import razie.diesel.dom.RDOM._
import razie.diesel.dom._
import razie.diesel.ext._
import razie.tconf.parser.{FoldingContext, LazyState, SState}
import razie.tconf.{DSpec, DUser}
import razie.wiki.Enc

import scala.Option.option2Iterable
import scala.util.Try
import scala.util.parsing.input.Positional

/** domain parser - for domain sections in a wiki */
trait DomParser extends ParserBase with ExprParser {

  import RDOM._

  def domainBlocks =
    pobject | pclass | passoc | pfunc |
    pwhen | pflow | pmatch | psend | pmsg | pval | pexpect

  def lazys (f:(SState, FoldingContext[DSpec,DUser]) => SState) =
    LazyState[DSpec, DUser] (f)

  // todo replace $ with . i.e. .class

  // ----------------------

  def optKinds: PS = opt(ows ~> "[" ~> ows ~> repsep(ident, ",") <~ "]") ^^ {
    case Some(tParm) => tParm.mkString
    case None => ""
  }

  /**
    * .class X [T] (a,b:String) extends A,B {}
    */
  def pclass: PS =
    """[.$]class""".r ~> ws ~>
      ident ~ opt(ows ~> "[" ~> ows ~> repsep(ident, ",") <~ "]") ~ optAttrs ~
      opt(ws ~> "extends" ~> ws ~> repsep(ident, ",")) ~
      opt(ws ~> "<" ~> ows ~> repsep(ident, ",") <~ ">") ~ " *".r ~ optClassBody ^^ {
      case name ~ tParm ~ attrs ~ ext ~ stereo ~ _ ~ funcs => {
        val c = C(name, "", stereo.map(_.mkString).mkString,
          ext.toList.flatMap(identity),
          tParm.map(_.mkString).mkString,
          attrs,
          funcs)
        lazys { (current, ctx) =>
          collectDom(c, ctx.we)

          def mkList = s"""<a href="/diesel/list2/${c.name}">list</a>"""

          // todo delegate decision to tconf domain - when domain is refactored into tconf
          def mkNew = "User" != name && "WikiLink" != name
//            if (ctx.we.exists(w => WikiDomain.canCreateNew(w.specPath.realm.mkString, name))) s""" | <a href="/doe/diesel/create/${c.name}">new</a>""" else ""

          SState(
            s"""
               |<div align="right"><small>$mkList $mkNew </small></div>
               |<div class="well">
               |$c
               |</div>""".stripMargin)
        }
      }
    }

  /** assoc : role */
  def assRole: Parser[(String, String)] = ident ~ " *: *".r ~ ident ^^ {
    case cls ~ _ ~ role => (cls, role)
  }

  /** assoc : role */
  def justAttrs: Parser[(String, String, List[RDOM.P])] = attrs ^^ {
    case a => ("", "", a)
  }

  /** p.a.ent.met - qualified ent so at least two elements
    *
    * @return (ent, act, fullString) */
  def qualified(qcm: List[String]): (String, String, String) = {
    val ea = qcm.mkString(".") //ent+"."+ac
    val ent = qcm.dropRight(1).mkString(".")
    val act = qcm.takeRight(1).mkString //ent+"."+ac
    (ent, act, ea)
  }

  /** ent.met - qualified ent so at least two elements
    *
    * @return (ent, act, fullString) */
  def qclsMet: Parser[(String, String, String)] = (ident | "*" | jsregex) ~ "." ~ (ident | "*" | jsregex) ~ rep("." ~> (ident | "*" | jsregex)) ^^ {
    case i ~ _ ~ j ~ l => {
      qualified(i :: j :: l)
    }
  }

  /** ent.met */
  def clsMet: Parser[(String, String, List[RDOM.P])] = (ident | "*" | jsregex) ~ " *. *".r ~ (ident | "*" | jsregex) ~ rep("." ~> (ident | "*" | jsregex)) ~ optAttrs ^^ {
    case i ~ _ ~ j ~ l ~ a => {
      val qcm = qualified(i :: j :: l)
      (qcm._1, qcm._2, a)
    }
  }

  def jsregex: P = """/[^/]*/""".r

  /** pattern match for ent.met */
  def clsMatch: Parser[(String, String, List[RDOM.PM])] = (ident | "*" | jsregex) ~ " *. *".r ~ (ident | "*" | jsregex) ~ rep("." ~> (ident | "*" | jsregex)) ~ optMatchAttrs ^^ {
    case i ~ _ ~ j ~ l ~ a => {
      val qcm = qualified(i :: j :: l)
      (qcm._1, qcm._2, a)
    }
  }

  /**
    * add a domain element to the topic
    */
  def addToDom(c: Any) = {
    lazys { (current, ctx) =>
      collectDom(c, ctx.we)
      SState(
        c match {
          case x: CanHtml => x.toHtml
          case s@_ =>
            //          """<span class="label label-primary">""" +
            """<p class="bg-info">""" +
              s.toString +
              """</p>"""
        }
      )
    }
  }

  /**
    */
  def pif: Parser[EIf] =
    """[.$]if""".r ~> ows ~> optMatchAttrs ^^ {
      case aa => EIf(aa)
    }

  /**
    * .match a.role (attrs)  // not used
    */
  def pmatch: PS =
    keyw("""[.$]match""".r) ~ ws ~ clsMatch ~ opt(pif) ^^ {
      case k ~ _ ~ Tuple3(ac, am, aa) ~ cond => {
        lazys { (current, ctx) =>
          val x = EMatch(ac, am, aa, cond)
          //          f.pos = Some(EPos(ctx.we.map(_.wid.wpath).mkString, k.pos.line, k.pos.column))
          addToDom(x).ifold(current, ctx)
        }
      }
    }

  def pArrow: Parser[String] = "=>" | "==>" ^^ {
    case s => s
  }

  /**
    * => z.role (attrs)
    **/
  def pgen: Parser[EMap] =
    ows ~> keyw(pArrow) ~ ows ~ opt(pif) ~ ows ~ (clsMet | justAttrs) ^^ {
      case arrow ~ _ ~ cond ~ _ ~ Tuple3(zc, zm, za) => {
        EMap(zc, zm, za, arrow.s, cond).withPosition(EPos("", arrow.pos.line, arrow.pos.column))
        // EPos wpath set later
      }
    }

  /**
    * .when a.role (attrs) => z.role (attrs)
    */
  def pwhen: PS =
    keyw("""[.$]when|[.$]mock""".r) ~ ws ~ clsMatch ~ ws ~ opt(pif) ~ rep(pgen) ^^ {
      case k ~ _ ~ Tuple3(ac, am, aa) ~ _ ~ cond ~ gens => {
        lazys { (current, ctx) =>
          val x = EMatch(ac, am, aa, cond)
          val wpath = ctx.we.map(_.specPath.wpath).mkString
          val r = ERule(x, gens.map(m=>m.withPosition(m.pos.get.copy(wpath=wpath))))
          r.pos = Some(EPos(wpath, k.pos.line, k.pos.column))
          val f = if (k.s contains "when") r else EMock(r)
          addToDom(f).ifold(current, ctx)
        }
      }
    }

  /**
    * .flow e.a => expr
    */
  def pflow: PS =
    keyw("""[.$]flow""".r) ~ ws ~ clsMatch ~ ws ~ opt(pif) ~ " *=>".r ~ ows ~ flowexpr ^^ {
      case k ~ _ ~ Tuple3(ac, am, aa) ~ _ ~ cond ~ _ ~ _ ~ ex => {
        lazys { (current, ctx) =>
          val x = EMatch(ac, am, aa, cond)
          val f = EFlow(x, ex)
          f.pos = Some(EPos(ctx.we.map(_.specPath.wpath).mkString, k.pos.line, k.pos.column))
          addToDom(f).ifold(current, ctx)
        }
      }
    }

  /**
    * .assoc name a:role -> z:role
    */
  def passoc: PS =
    """[.$]assoc""".r ~> ws ~> opt(ident <~ ws) ~ assRole ~ " *-> *".r ~ assRole ~ optAttrs ^^ {
      case n ~ Tuple2(a, arole) ~ _ ~ Tuple2(z, zrole) ~ p => {
        val c = A(n.mkString, a, z, arole, zrole, p)
        lazys { (current, ctx) =>
          collectDom(c, ctx.we)
          SState(
            """<span class="label label-default">""" +
              c.toString +
              """</span>""")
        }
      }
    }

  def pobject: PS =
    """[.$]object """.r ~> ident ~ " *".r ~ ident ~ opt(CRLF2 ~> rep1sep(vattrline, CRLF2)) ^^ {
      case name ~ _ ~ c ~ l => {
        val o = O(name, c, l.toList.flatMap(identity))
        lazys { (current, ctx) =>
          collectDom(o, ctx.we)
          SState(
            """<div class="well">""" +
              s"object $name (" + l.mkString(", ") +
              ")" +
              """</div>""")
        }
      }
    }

  /**
    * optional attributes
    */
  def attrs: Parser[List[RDOM.P]] = " *\\(".r ~> ows ~> repsep(pattr, ows ~ "," ~ ows) <~ ows <~ ")"

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
  def optMatchAttrs: Parser[List[RDOM.PM]] = opt(" *\\(".r ~> ows ~> repsep(pmatchattr, "," ~ ows) <~ ows <~ ")") ^^ {
    case Some(a) => a
    case None => List.empty
  }

  // ?
  def attr: Parser[_ >: CM] = pattr

  /**
    * :<>type[kind]*
    * <> means it's a ref, not ownership
    * * means it's a list
    */
  def optType: Parser[String] = opt(" *: *".r ~> ident ~ optKinds ~ opt(" *\\* *".r)) ^^ {
    case Some(tt ~ k ~ multi) => tt + k.s + multi.mkString
    case None => ""
  }

  def OPS1: Parser[String] = "==|~=|!=|\\?=|>=|<=|>|<|contains|is|not".r

  /**
    * name:<>type[kind]*=default
    * <> means it's a ref, not ownership
    * means it's a list
    *
    * pmatch is more than just a simple conditional expression
    */
  def pmatchattr: Parser[RDOM.PM] = " *".r ~> qident ~ opt(" *: *".r ~> opt("<>") ~ ident ~ optKinds) ~ opt(" *\\* *".r) ~ opt(" *".r ~> OPS1 ~ " *".r ~ expr) ^^ {
    case name ~ t ~ multi ~ e => {
      var ttype = ""
      var dflt = ""
      val exp = e match {
        case Some(op ~ _ ~ v) => {
          if(v.isInstanceOf[CExpr]) ttype = v.asInstanceOf[CExpr].ttype
          (op, Some(v))
        }
        case None => ("", None)
      }
      t match {
        case Some(Some(ref) ~ tt ~ k) => PM(name, tt + k.s, ref, multi.mkString, exp._1, dflt, exp._2)
        case Some(None ~ tt ~ k) => PM(name, tt + k.s, "", multi.mkString, exp._1, dflt, exp._2)
        case None => PM(name, ttype, "", multi.mkString, exp._1, dflt, exp._2)
      }
    }
  }

  /**
    * name:<>type[kind]*=default
    * <> means it's a ref, not ownership
    * * means it's a list
    */
  def pattr: Parser[RDOM.P] = " *".r ~> qident ~ opt(" *: *".r ~> opt("<>") ~ ident ~ optKinds) ~ opt(" *\\* *".r) ~ opt(" *~?= *".r ~> expr) ^^ {
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


  // value assignment
  def vattrline: Parser[V] = " *".r ~> ident ~ " *= *".r ~ any ^^ {
    case name ~ _ ~ v => V(name, v)
  }

  def optClassBody: Parser[List[RDOM.F]] = opt(" *\\{".r ~> CRLF2 ~> rep1sep(fattrline, CRLF2) <~ CRLF2 <~ " *\\} *".r) ^^ {
    case Some(a) => a
    case None => List.empty
  }

  /**
    * .option name:type=value
    *
    * use them to set options
    */
  def pval: PS = "[.$]val *".r ~> pattr ^^ {
    case a => {
      lazys { (current, ctx) =>
        val v = EVal(a)
        collectDom(v, ctx.we)
        SState(v.toHtml)
      }
    }
  }

  case class Keyw(s: String) extends Positional

  private def keyw(r: Parser[String]) = positioned(r.map(s => Keyw(s)))

  private def keyw(r: scala.util.matching.Regex) = positioned(pkeyw(r))

  private def pkeyw(r: scala.util.matching.Regex): Parser[Keyw] = r ^^ {
    case s => Keyw(s)
  }

  /**
    * .receive object.func (a,b)
    *
    * An NVP is either the spec or an instance of a function call, a message, a data object... whatever...
    */
  def psend: PS = keyw("[.$]send *".r) ~ opt("<" ~> "[^>]+".r <~ "> *".r) ~ qclsMet ~ optAttrs ~ opt(" *: *".r ~> optAttrs) <~ " *".r ^^ {
    case k ~ stype ~ qcm ~ attrs ~ ret => {
      lazys { (current, ctx) =>
        val f = EMsg("receive", qcm._1, qcm._2, attrs, ret.toList.flatten(identity), stype.mkString.trim)
        f.pos = Some(EPos(ctx.we.map(_.specPath.wpath).mkString, k.pos.line, k.pos.column))
        collectDom(f, ctx.we)
        SState(f.kspan("receive::") + f.toHtmlInPage + "<br>")
      }
    }
  }

  /**
    * .msg object.func (a,b)
    *
    * An NVP is either the spec or an instance of a function call, a message, a data object... whatever...
    */
  def pmsg: PS = keyw("[.$]msg *".r) ~ opt("<" ~> "[^>]+".r <~ "> *".r) ~ qclsMet ~ optAttrs ~ opt(" *(:|=>) *".r ~> optAttrs) <~ " *".r ^^ {
    case k ~ stype ~ qcm ~ attrs ~ ret => {
      lazys { (current, ctx) =>

        val archn =
          if (stype.exists(_.length > 0)) stype.mkString.trim
          else {
            // todo snakkers need to be plugged in and insulated better
            // if no archetype specified, find a template snakker and import stype
            val t = ctx.we.flatMap(_.findTemplate(qcm._3))
            val sc = t.map(_.content).mkString
            if ("" != sc) Try {
              EESnakk.parseTemplate(t, sc, attrs).method
            }.getOrElse("") else ""
          }

        val f = EMsg("def", qcm._1, qcm._2, attrs, ret.toList.flatten(identity), archn)

        f.pos = Some(EPos(ctx.we.map(_.specPath.wpath).mkString, k.pos.line, k.pos.column))
        collectDom(f, ctx.we)
        SState(f.toHtmlInPage + "<br>")
      }
    }
  }

  /**
    * .msg object.func (a,b)
    *
    * An NVP is either the spec or an instance of a function call, a message, a data object... whatever...
    */
  def linemsg(wpath: String) = keyw("[.$]msg *".r | "[.$]send\\s*".r) ~ opt("<" ~> "[^>]+".r <~ "> *".r) ~ ident ~ " *\\. *".r ~ qident ~ optAttrs ~ opt(" *: *".r ~> optAttrs) ^^ {
    case k ~ stype ~ ent ~ _ ~ ac ~ attrs ~ ret => {
      val f = EMsg("def", ent, ac, attrs, ret.toList.flatten(identity), stype.mkString.trim)
      f.pos = Some(EPos(wpath, k.pos.line, k.pos.column))
      f
    }
  }

  /**
    * .mock a.role (attrs) => z.role (attrs)
    */
  def linemock(wpath: String) =
    keyw("""[.$]mock""".r) ~ ws ~ clsMatch ~ ws ~ opt(pif) ~ pgen  <~ " *".r ^^ {
      case k ~ _ ~ Tuple3(ac, am, aa) ~ _ ~ cond ~ gen => {
        val x = EMatch(ac, am, aa, cond)
        val f = EMock(ERule(x, List(gen)))
        f.pos = Some(EPos(wpath, k.pos.line, k.pos.column))
        f.rule.pos = Some(EPos(wpath, k.pos.line, k.pos.column))
        f
      }
    }

  /**
    * .expect object.func (a,b)
    */
  def pexpect: PS = keyw("[.$]expect".r <~ ws) ~ opt("not" <~ ws) ~ opt(qclsMet) ~ optMatchAttrs ~ opt(pif) <~ " *".r ^^ {
    case k ~ not ~ qcm ~ attrs ~ cond => {
      lazys { (current, ctx) =>
        val pos = Some(EPos(ctx.we.map(_.specPath.wpath).mkString, k.pos.line, k.pos.column))
        val f = qcm.map(qcm =>
          ExpectM(not.isDefined, EMatch(qcm._1, qcm._2, attrs, cond)).withPos(pos))
          .getOrElse(ExpectV(not.isDefined, attrs).withPos(pos))
        collectDom(f, ctx.we)
        SState(f.toHtml + "<br>")
      }
    }
  }

  private def collectDom(x: Any, we: Option[DSpec]) = {
    we.foreach { w =>
      val rest = w.collector.getOrElse(RDomain.DOM_LIST, List[Any]()).asInstanceOf[List[Any]]
      w.collector.put(RDomain.DOM_LIST, x :: rest)
    }
  }

  /**
    * .func name (a,b) : String
    */
  def pfunc: PS = "[.$]def *".r ~> qident ~ optAttrs ~ opt(" *: *".r ~> ident) ~ optScript ~ optBlock ^^ {
    case name ~ a ~ t ~ s ~ b => {
      lazys { (current, ctx) =>
        val f = F(name, a, t.mkString, s.fold(ctx).s, b)
        collectDom(f, ctx.we)

        def mkParms = f.parms.map { p => p.name + "=" + Enc.toUrl(p.dflt) }.mkString("&")

        def mksPlay = if (f.script.length > 0) s""" | <a href="/diesel/splay/${f.name}/${ctx.we.map(_.specPath.wpath).mkString}?$mkParms">splay</a>""" else ""

        def mkjPlay = if (f.script.length > 0) s""" | <a href="/diesel/jplay/${f.name}/${ctx.we.map(_.specPath.wpath).mkString}?$mkParms">jplay</a>""" else ""

        def mkCall =
          s"""<a href="/diesel/fcall/${f.name}/${ctx.we.map(_.specPath.wpath).mkString}?$mkParms">fcall</a>$mkjPlay$mksPlay""".stripMargin

        SState(
          s"""
             |<div align="right"><small>$mkCall</small></div>
             |<div class="well">
             |$f
             |</div>""".stripMargin)
      }
    }
  }

  /** optional script body */
  def optScript: PS = opt(" *\\{\\{ *".r ~> lines <~ "}}") ^^ {
    case Some(lines) => lines
    case None => ""
  }

  /**
    * def name (a,b) : String
    */
  def fattrline: Parser[RDOM.F] = " *def *".r ~> ident ~ optAttrs ~ optType ~ " *".r ~ optBlock ^^ {
    case name ~ a ~ t ~ _ ~ b => F(name, a, t.mkString, "", b)
  }

  def optBlock: Parser[List[Executable]] = opt(" *\\{".r ~> CRLF2 ~> rep1sep(statement, CRLF2) <~ CRLF2 <~ " *\\} *".r) ^^ {
    case Some(a) => a
    case None => List.empty
  }

  def statement: Parser[Executable] = svalue | scall

  def svalue: Parser[Executable] = valueDef ^^ { case p => new ExecutableValue(p) }

  // not used yet - class member val
  def valueDef: Parser[RDOM.P] = "val *".r ~> ident ~ opt(" *: *".r ~> opt("<>") ~ ident) ~ opt(" *\\* *".r) ~ opt(" *= *".r ~> value) ^^ {
    case name ~ t ~ multi ~ e => t match {
      case Some(Some(ref) ~ tt) => P(name, e.mkString, tt, ref, multi.mkString)
      case Some(None ~ tt) => P(name, e.mkString, tt.mkString, "", multi.mkString)
      case None => P(name, e.mkString, "", "", multi.mkString)
    }
  }

  // not used yet - class member val
  def scall: Parser[Executable] = ows ~> ident ~ "." ~ ident ~ optAttrs ^^ {
    case cls ~ _ ~ func ~ attres =>
      new ExecutableCall(cls, func, attres)
  }

  private def trim(s: String) = s.replaceAll("\r", "").replaceAll("^\n|\n$", "") //.replaceAll("\n", "\\\\n'\n+'")

}

class ExecutableValue(p: RDOM.P) extends Executable {
  def sForm = "val " + p.toString

  def exec(ctx: Any, parms: Any*): Any = ""
}

class ExecutableCall(cls: String, func: String, args: List[P]) extends Executable {
  def sForm = s"call $cls.$func (${args.mkString})"

  def exec(ctx: Any, parms: Any*): Any = ""
}
