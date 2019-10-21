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
import razie.diesel.engine.DomEngine
import razie.diesel.expr.{BExpr, CExpr}
import razie.diesel.ext._
import razie.tconf.parser.{FoldingContext, LazyAstNode, StrAstNode, TriAstNode}
import razie.tconf.{DSpec, DUser, EPos}
import razie.wiki.Enc
import scala.Option.option2Iterable
import scala.concurrent.Future
import scala.util.Try
import scala.util.parsing.input.Positional

/** domain parser - for domain sections in a wiki */
trait DomParser extends ParserBase with ExprParser {

  // resolve an override resulting when I simplified ExprParser
  override type P = Parser[String]
  override def ws = whiteSpace
  override def ows = opt(whiteSpace)

  import RDOM._

  def domainBlocks =
    panno |  pobject | pclass | passoc | pdef |
    pwhen | pflow | pmatch | psend | pmsg | pval | pexpect | passert

  def lazys (f:(StrAstNode, FoldingContext[DSpec,DUser]) => StrAstNode) =
    LazyAstNode[DSpec, DUser] (f)

  // todo replace $ with . i.e. .class

  // ----------------------

  /**
    * .anno (params)
    *
    * annotation - applied to the next element. you can have just one for now
    *
    * annotations have to be in the same page and are claimed by the first element that follows
    */
  def panno: PS =
    """[.$]anno(tate)? +""".r ~> ows ~> optAttrs ^^ {
      case attrs => {
        lazys { (current, ctx) =>
          // was it collected? if so, merge the two defs
          ctx.we.foreach { w =>
            w.collector.put(RDomain.DOM_ANNO_LIST, attrs)
          }

          StrAstNode(
            s"""${span("anno")} ${mksAttrs(attrs)}
               """.stripMargin)
        }
      }
    }

  /**
    * .class X [T] (a,b:String) extends A,B {}
    */
  def pclass: PS =
    """[.$]class""".r ~> ws ~>
      qident ~ opt(ows ~> "[" ~> ows ~> repsep(qident, ",") <~ "]") ~
       optAttrs ~
      opt(ws ~> "extends" ~> ws ~> repsep(qident, ",")) ~
      opt(ws ~> "<" ~> ows ~> repsep(ident, ",") <~ ">") ~ " *".r ~ optClassBody ^^ {
      case name ~ tParm ~ attrs ~ ext ~ stereo ~ _ ~ funcs => {
        lazys { (current, ctx) =>

          val anno = ctx.we.get.collector.getOrElse(RDomain.DOM_ANNO_LIST, Nil).asInstanceOf[List[RDOM.P]]
          ctx.we.get.collector.remove(RDomain.DOM_ANNO_LIST)

          var c = C(name, "", stereo.map(_.mkString).mkString,
            ext.toList.flatMap(identity),
            tParm.map(_.mkString).mkString,
            attrs,
            funcs,
            Nil,
            anno)

          var actions = ""

          // was it collected? if so, merge the two defs
          ctx.we.foreach { w =>
            val rest = w.collector.getOrElse(RDomain.DOM_LIST, List[Any]()).asInstanceOf[List[AnyRef]]


            val collected = rest.collect {
              case wc: C if wc.name == c.name && (wc.parms.size > 0 || wc.methods.size > 0) => wc
            }

            if(collected.size > 0) {
              w.collector.put(RDomain.DOM_LIST, rest.filterNot(wc=> collected.exists(x=> x.eq(wc))))
            }

            // collect only if not meaningfully defined before, so you can reference a class with jsut '$class xx'
            c = collected.foldLeft(c){(a,b) => a.plus(b)}
            collectDom(c, ctx.we)

            actions = RDomainPlugins.htmlActions(w.specPath.realm, c)
          }

          StrAstNode(
            s"""
               |<div align="right"><small>$actions </small></div>
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
  def justAttrs: Parser[List[PAS]] = pasattrs ^^ {
    case a => a
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

  /** ent.met (parms) */
  def clsMet: Parser[(String, String, List[RDOM.P])] = (ident | "*" | jsregex) ~ " *. *".r ~ (ident | "*" | jsregex) ~ rep("." ~> (ident | "*" | jsregex)) ~ opt(pcallattrs) ^^ {
    case i ~ _ ~ j ~ l ~ a => {
      val qcm = qualified(i :: j :: l)
      (qcm._1, qcm._2, a.toList.flatten)
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
      StrAstNode(
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
  def pif: Parser[EIf] = pifc | pifm

  /**
    * ifc is the default if
    */
  def pifc: Parser[EIf] =
    """[.$]?ifc?""".r ~> ows ~> notoptCond ^^ {
            case b : BExpr => EIfc(b)
    }

//  def pifc: Parser[EIf] =
//    """[.$]?ifc?""".r ~> ows ~> optCond ^^ {
//      case b : Option[BExpr] if b.isDefined => EIfc(b.get)
//      case _ => EIfc(BExprFALSE)
//    }

  /**
    */
  def pifm: Parser[EIf] =
    """[.$]?match""".r ~> ows ~> notoptMatchAttrs ^^ {
      case a : List[PM] => EIfm(a)
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

  def pComment: Parser[String] = " *//.*".r ^^ {
    case s => s
  }

  def optComment: Parser[String] = opt(pComment) ^^ {
    case s => s.mkString
  }

  /**
    * => z.role (attrs)
    */
  def pgen: Parser[EMap] =
    ows ~> keyw(pArrow) ~ ows ~ opt(pif) ~ ows ~ (clsMet | justAttrs) <~ opt(";") <~ optComment ^^ {
      case arrow ~ _ ~ cond ~ _ ~ cp => {

        cp match {

            // class with message
          case Tuple3(zc, zm, za) =>
            EMapCls(zc.toString, zm.toString, za.asInstanceOf[List[RDOM.P]], arrow.s, cond).withPosition(EPos("", arrow.pos.line, arrow.pos.column))

            // just parm assignments
          case pas:List[_] =>
            EMapPas(pas.asInstanceOf[List[PAS]], arrow.s, cond).withPosition(EPos("", arrow.pos.line, arrow.pos.column))
        }

        // EPos wpath set later
      }
    }

  /**
    * - text - i.e. step description
    */
  def pgenStep: Parser[EMap] =
    ows ~> keyw("-") ~ ows ~ opt(pif) ~ ows ~ "[^\n\r;]+".r <~ opt(";") <~ optComment ^^ {
      case arrow ~ _ ~ cond ~ _ ~ desc => {
        EMapCls("diesel", "step", List(P("desc", desc)), arrow.s, cond).withPosition(EPos("", arrow.pos.line, arrow.pos.column))
      }
    }

  /**
    * - text - i.e. step description
    * todo is not working
    */
  def pgenText: Parser[EMap] =
    ows ~> keyw("[^\n\r=\\-;]".r) ~ ows ~ opt(pif) ~ ows ~ "[^\n\r;]+".r <~ opt(";") ^^ {
      case arrow ~ _ ~ cond ~ _ ~ desc => {
        EMapCls("diesel", "step", List(P("desc", arrow.s+desc)), "-", cond).withPosition(EPos("", arrow.pos.line, arrow.pos.column))
      }
    }

  /**
    * this one used for fiddles, see FiddleParser
    *
    * this is not parsed in the context of a wiki, it's just for a fiddle display...
    */
  def fiddleBlocks (wpath:String) = linemsg(wpath) | linemock(wpath)
  def noFiddleB  = not("""[.$]mock""" | "[.$]send")
  def fiddleLines (wpath:String) =
    rep(fiddleBlocks(wpath) | (fiddleBlocks(wpath) ~ CRLF2) | ( (noFiddleB | "") ~ ".*".r ~ (CRLF1 | CRLF3 | CRLF2))) ^^ {
    case l => l
  }

  /**
    * this one used for fiddles, see FiddleParser
    *
    * this is not parsed in the context of a wiki, it's just for a fiddle display...
    *
    * .mock a.role (attrs) => z.role (attrs)
    */
  def linemock(wpath: String) =
    keyw("""[.$]mock""".r) ~ ws ~ optArch ~ clsMatch ~ ws ~ opt(pif) ~ rep(pgen | pgenStep) ^^ {
      case k ~ _ ~ oarch ~ Tuple3(ac, am, aa) ~ _ ~ cond ~ gen => {
        val x = EMatch(ac, am, aa, cond)
        val f = EMock(ERule(x, "mock", gen))
        f.pos = Some(EPos(wpath, k.pos.line, k.pos.column))
        f.rule.pos = Some(EPos(wpath, k.pos.line, k.pos.column))
        f
      }
    }

  /**
    * .when <tags> a.role (attrs) => z.role (attrs)
    * tags are optional and could be rule, mock, model, impl etc
    * - rule is the default for execution
    * - mock is for mocks
    * - others like model or impl are specific
    */
  def pwhen: PS =
    keyw("""[.$]when|[.$]mock""".r) ~ ws ~ optArch ~ clsMatch ~ ws ~ opt(pif) ~ optComment ~ rep(pgen | pgenStep ) ^^ {
      case k ~ _ ~ oarch ~ Tuple3(ac, am, aa) ~ _ ~ cond ~ _ ~ gens => {
        lazys { (current, ctx) =>
          val x = EMatch(ac, am, aa, cond)
          val wpath = ctx.we.map(_.specPath.wpath).mkString
          val arch = oarch.filter(_.length > 0).getOrElse(k.s) // archetype
          val r = ERule(x, arch, gens.map(m=>m.withPosition(m.pos.get.copy(wpath=wpath))))
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
          StrAstNode(
            """<span class="label label-default">""" +
              c.toString +
              """</span>""")
        }
      }
    }

  // what is the second ident - the class? examples?
  // $object ha.haha.ha sdfsdfsdf (asdfasdfasdf)
  def pobject: PS =
    keyw("""[.$]object """.r) ~> ident ~ " *".r ~ ident ~ optAttrs ^^ {
      case name ~ _ ~ cls ~ l => {
        val o = O(name, cls, l)
        lazys { (current, ctx) =>
          collectDom(o, ctx.we)
          StrAstNode(
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
  def XXattrs: Parser[List[RDOM.P]] = " *\\(".r ~> ows ~> repsep(pattr, ows ~ "," ~ ows) <~ ows <~ ")"

  /**
    * optional attributes
    */
  def XXoptAttrs: Parser[List[RDOM.P]] = opt(attrs) ^^ {
    case Some(a) => a
    case None => List.empty
  }

  /**
    * optional attributes
    */
  private def optMatchAttrs: Parser[List[RDOM.PM]] = opt(" *\\(".r ~> ows ~> repsep(pmatchattr, ows ~> "," ~ ows) <~ ows <~ ")") ^^ {
    case Some(a) => a
    case None => List.empty
  }

  /**
    * attributes
    */
  private def notoptMatchAttrs: Parser[List[RDOM.PM]] = " *\\(".r ~> ows ~> repsep(pmatchattr, ows ~> "," ~ ows <~ ows <~ ")") ^^ {
    case a => a
  }

  /**
    * condition / expression
    */
  private def notoptCond: Parser[BExpr] = " *\\(".r ~> ows ~> cond <~ ows <~ ")" ^^ {
    case x => x
  }

  /**
    * optional condition / expression
    */
  private def optCond: Parser[Option[BExpr]] = opt(" *\\(".r ~> ows ~> cond <~ ows <~ ")") ^^ {
    case x => x
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
  def pmatchattr: Parser[RDOM.PM] = ows ~>
      (aidentaccess | aident) ~
      opt(ows ~> ":" ~> ows ~> opt("<>") ~ ident ~ optKinds) ~
      opt(" *\\* *".r) ~
      opt(ows ~> OPS1 ~ ows ~ expr) ^^ {

    case ident ~ t ~ multi ~ e => {
      var ttype = ""
      var dflt = ""
      val exp = e match {
        case Some(op ~ _ ~ v) => {
          if(v.isInstanceOf[CExpr[_]]) ttype = v.asInstanceOf[CExpr[_]].ttype
          (op, Some(v))
        }
        case None => ("", None)
      }
      t match {
        case Some(Some(ref) ~ tt ~ k) => PM(ident, WType(tt + k.s), ref, multi.mkString, exp._1, dflt, exp._2)
        case Some(None ~ tt ~ k) => PM(ident, WType(tt + k.s), "", multi.mkString, exp._1, dflt, exp._2)
        case None => PM(ident, WType(ttype), "", multi.mkString, exp._1, dflt, exp._2)
      }
    }
  }

 def optClassBody: Parser[List[RDOM.F]] = opt(" *\\{".r ~> CRLF2 ~> rep1sep(defline | msgline, CRLF2) <~ CRLF2 <~ " *\\} *".r) ^^ {
    case Some(a) => a
    case None => List.empty
  }

  /**
    * .option name:type=value
    *
    * use them to set options
    */
  def pval: PS = keyw("[.$]val *".r) ~ pattr ^^ {
    case k ~ a => {
      lazys { (current, ctx) =>
        val v = EVal(a).withPos(pos(k))
        collectDom(v, ctx.we)
        StrAstNode(v.toHtml)
      }
    }
  }

  case class Keyw(s: String) extends Positional

  private def keyw(r: Parser[String]) = positioned(r.map(s => Keyw(s)))

  private def keyw(r: scala.util.matching.Regex) = positioned(pkeyw(r))

  private def pkeyw(r: scala.util.matching.Regex): Parser[Keyw] = r ^^ {
    case s => Keyw(s)
  }

  def pos (k:Keyw, ctx:FoldingContext[DSpec,DUser]) = {
    Some(EPos(ctx.we.map(_.specPath.wpath).mkString, k.pos.line, k.pos.column))
  }

  def pos (k:Keyw) = {
    Some(EPos("", k.pos.line, k.pos.column))
  }

  private def lastMsg(we: Option[DSpec]) = {
    we.flatMap { w =>
      val rest = w.collector.getOrElse(RDomain.DOM_LIST, List[Any]()).asInstanceOf[List[Any]]
      // important to be at the front...
      rest.find(_.isInstanceOf[EMsg]).map(_.asInstanceOf[EMsg])
    }
  }

  /**
    * .receive object.func (a,b)
    *
    * An NVP is either the spec or an instance of a function call, a message, a data object... whatever...
    */
  def psend: PS = keyw("[.$]send *".r) ~ opt("<" ~> "[^>]+".r <~ "> *".r) ~ qclsMet ~ optAttrs ~ opt(" *: *".r ~> optAttrs) <~ " *".r <~ optComment ^^ {
    case k ~ stype ~ qcm ~ attrs ~ ret => {
      lazys { (current, ctx) =>
        val f = EMsg(qcm._1, qcm._2, attrs, "send", ret.toList.flatten(identity), stype.mkString.trim)
        f.pos = pos(k, ctx)
        collectDom(f, ctx.we)
        StrAstNode(f.kspan("send::") + f.toHtmlInPage + "<br>")
      }
    }
  }

  private def optArch : Parser[Option[String]] = opt("<" ~> "[^>]+".r <~ "> *".r) ^^ {
    case s => s
  }

  /**
    * .msg object.func (a,b)
    *
    * An NVP is either the spec or an instance of a function call, a message, a data object... whatever...
    */
  def pmsg: PS = keyw("[.$]msg *".r) ~ optArch ~ qclsMet ~ optAttrs ~ opt(" *(:|=>) *".r ~> optAttrs) <~ " *".r <~ optComment ^^ {
    case k ~ stype ~ qcm ~ attrs ~ ret => {
      lazys { (current, ctx) =>

        val archn =
          if (stype.exists(_.length > 0)) stype.mkString.trim
          else {
            // todo snakkers need to be plugged in and insulated better
            // if no archetype specified, find a template snakker and import stype
            val t = ctx.we.flatMap(_.findSection(qcm._3))
            val sc = t.map(_.content).mkString
            if ("" != sc) Try {
              EESnakk.parseTemplate(t, sc, attrs).method
            }.getOrElse("") else ""
          }

        val anno = ctx.we.get.collector.getOrElse(RDomain.DOM_ANNO_LIST, Nil).asInstanceOf[List[RDOM.P]]
        ctx.we.get.collector.remove(RDomain.DOM_ANNO_LIST)

        val f = EMsg(qcm._1, qcm._2, attrs, "def", ret.toList.flatten(identity), archn)

        f.pos = Some(EPos(ctx.we.map(_.specPath.wpath).mkString, k.pos.line, k.pos.column))
        collectDom(f, ctx.we)
        StrAstNode(f.toHtmlInPage + "<br>")
      }
    }
  }

  /**
    * this one used for fiddles, see FiddleParser
    *
    * just for display, not in the context of a wiki
    *
    * .msg object.func (a,b) : (out)
    */
  def linemsg(wpath: String) = keyw("[.$]msg *".r | "[.$]send\\s*".r) ~ opt("<" ~> "[^>]+".r <~ "> *".r) ~ ident ~ " *\\. *".r ~ qident ~ optAttrs ~ opt(" *: *".r ~> optAttrs) ^^ {
    case k ~ stype ~ ent ~ _ ~ ac ~ attrs ~ ret => {
      val f = EMsg(ent, ac, attrs, "def", ret.toList.flatten(identity), stype.mkString.trim)
      f.pos = Some(EPos(wpath, k.pos.line, k.pos.column))
      f
    }
  }

  /**
    * .expect object.func (a,b)
    */
  def pexpect: PS = keyw("[.$]expect".r <~ ws) ~
      opt("not" <~ ws) ~
      opt(pif) ~ ows ~
      opt(qclsMet) ~ optMatchAttrs ~ " *".r ~ opt(pif) <~ " *".r <~ optComment ^^ {
    case k ~ not ~ pif ~ _ ~ qcm ~ attrs ~ _ ~ cond => {
      lazys { (current, ctx) =>
        val pos = Some(EPos(ctx.we.map(_.specPath.wpath).mkString, k.pos.line, k.pos.column))
        val f = qcm.map(qcm =>
          ExpectM(not.isDefined, pif, EMatch(qcm._1, qcm._2, attrs, cond))
              .withPos(pos)
              .withGuard(lastMsg(ctx.we).map(_.asMatch))
        ).getOrElse(
          ExpectV(not.isDefined, attrs, cond)
                .withPos(pos)
                .withGuard(lastMsg(ctx.we).map(_.asMatch))
          )
        collectDom(f, ctx.we)
        StrAstNode(f.toHtml + "<br>")
      }
    }
  }

  /**
    * todo is not working
    */
  def optAssertExprs: Parser[List[BExpr]] = ows ~> cond <~ ows ^^ {
    case x => x :: Nil
  }

  /**
    * .expect object.func (a,b)
    */
  def passert: PS = keyw("[.$]assert".r <~ ws) ~ opt("not" <~ ws) ~ optAssertExprs <~ " *".r <~ optComment ^^ {
    case k ~ not ~ exprs => {
      lazys { (current, ctx) =>
        val pos = Some(EPos(ctx.we.map(_.specPath.wpath).mkString, k.pos.line, k.pos.column))
        val f = ExpectAssert(not.isDefined, exprs).withPos(pos)
        collectDom(f, ctx.we)
        StrAstNode(f.toHtml + "<br>")
      }
    }
  }

  private def collectDom(x: Any, we: Option[DSpec]) = {
    we.foreach { w =>
      val rest = w.collector.getOrElse(RDomain.DOM_LIST, List[Any]()).asInstanceOf[List[Any]]
      // important to be at the front...
      w.collector.put(RDomain.DOM_LIST, x :: rest)
    }
  }

  /**
    * .func name (a,b) : String
    */
  def pdef: PS = "[.$]def *".r ~> qident ~ optAttrs ~ opt(" *: *".r ~> ident) ~ optScript ~ optBlock ^^ {
    case name ~ attrs ~ optType ~ script ~ block => {
      lazys { (current, ctx) =>
        val f = F(name, attrs, optType.mkString, "def", script.fold(ctx).s, block)
        collectDom(f, ctx.we)

        def mkParms = f.parms.map { p => p.name + "=" + Enc.toUrl(p.currentStringValue) }.mkString("&")

        def mksPlay = if (f.script.length > 0) s""" | <a href="/diesel/splay/${f.name}/${ctx.we.map(_.specPath.wpath).mkString}?$mkParms">splay</a>""" else ""

        def mkjPlay = if (f.script.length > 0) s""" | <a href="/diesel/jplay/${f.name}/${ctx.we.map(_.specPath.wpath).mkString}?$mkParms">jplay</a>""" else ""

        def mkCall =
          s"""<a href="/diesel/fcall/${f.name}/${ctx.we.map(_.specPath.wpath).mkString}?$mkParms">fcall</a>$mkjPlay$mksPlay""".stripMargin

        StrAstNode(
          s"""
             |<div align="right"><small>$mkCall</small></div>
             |<div class="well">
             |$f $script
             |</div>""".stripMargin)
      }
    }
  }

  /** optional script body */
  // using positive exlusive lookahead and .*? is not-greedy
  def optScript: PS = opt(" *\\{\\{ *".r ~> "(?s).*?(?=}})".r <~ "}}") ^^ {
    case Some(lines) => lines
    case None => ""
  }

  /** optional script body */
  // using positive exlusive lookahead and .*? is not-greedy
  def pblock: PS = " *\\{\\{ *".r ~> "(?s).*?(?=}})".r <~ "}}" ^^ {
    case lines => lines
      // todo return a lambda JS executable
  }

  /**
    * msg name (a,b) : String
    */
  def defline: Parser[RDOM.F] = " *\\$?XXdef *".r ~> ident ~ optAttrs ~ optType ~ " *".r ~ optBlock ^^ {
    case name ~ a ~ t ~ _ ~ b => {
      new F(name, a, t.mkString, "def", "", b)
    }
  }

  /**
    * def name (a,b) : String
    */
  def msgline: Parser[RDOM.F] = " *\\$?msg *".r ~> ident ~ optAttrs ~ optType ~ " *".r ~ opt(pgen) ^^ {
    case name ~ a ~ t ~ _ ~ m => {
      new F(name, a, t.mkString, "msg", "", m.toList.map(x=>new ExecutableMsg(x)))
    }
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

class ExecutableValue(p: RDOM.P) extends ExecutableSync {
  def sForm = "val " + p.toString

  def exec(ctx: Any, parms: Any*): Any = ""
}

class ExecutableCall(cls: String, func: String, args: List[P]) extends ExecutableSync {
  def sForm = s"call $cls.$func (${args.mkString})"

  def exec(ctx: Any, parms: Any*): Any = ""
}

class ExecutableMsg(m:EMap) extends ExecutableAsync {
  def sForm = m.toHtml

  override def start(ctx: Any, inEngine:Option[DomEngine]): Future[DomEngine] = ???
}

