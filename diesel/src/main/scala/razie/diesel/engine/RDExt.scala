/**
 *  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import java.util.regex.Pattern

import mod.diesel.model.exec._
import razie.Snakk
import razie.diesel.dom.RDOM._
import razie.diesel.dom.{RDomain, _}
import razie.diesel.exec.{EEFunc, EETest}
import razie.diesel.ext._
import razie.xp.JsonOWrapper

import scala.Option.option2Iterable
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

/** accumulate results and infos and errors */
class InfoAccumulator (var eres : List[Any] = Nil) {

  def += (x:Any) = append(x)

  // todo - weird match/case instead of explicit Seq
  def append (x:Any) = {
    x match {
      case l : List[_] => eres = eres ::: l
      case _ => eres = eres ::: x :: Nil
    }
    eres
  }
}

/** RDOM extensions */
object RDExt {

  def init = {
    new EECtx ::
      new EEDieselMemDb ::
      new EEDieselSharedDb ::
      new EESnakk ::
      new EETest ::
      new EEFunc :: Nil map Executors.add
  }

  /** parse from/to json utils */
  object DieselJsonFactory {
    private def sm(x:String, m:Map[String, Any]) : String = if(m.contains(x)) m(x).asInstanceOf[String] else ""

    def fromj (o:Map[String, Any]) : Any = {
      def s(x:String)  : String = sm(x, o)
      def l(x:String) = if(o.contains(x)) o(x).asInstanceOf[List[_]] else Nil

      def parms (name:String) = l(name).collect {
        case m:Map[String, Any] => P(sm("name",m), sm("value",m))
      }

      if(o.contains("class")) s("class") match {
        case "EMsg" => EMsg(
          s("arch"),
          s("entity"),
          s("met"),
          parms("attrs"),
          parms("ret"),
          s("stype")
        ).withPos(if(o.contains("pos")) Some(new EPos(o("pos").asInstanceOf[Map[String, Any]])) else None)

        case "EVal" => EVal (P(
          s("name"), s("value")
        )).withPos(if(o.contains("pos")) Some(new EPos(o("pos").asInstanceOf[Map[String, Any]])) else None)

        case "DomAst" => {
          val c = l("children").collect {
            case m:Map[String, Any] => fromj(m).asInstanceOf[DomAst]
          }

          val v = o("value") match {
            case s:String => s
            case m : Map[String,Any] => DieselJsonFactory.fromj(m)
            case x@_ => x.toString
          }

          val d = DomAst(
            v,
            s("kind"),
            (new ListBuffer[DomAst]()) ++ c,
            s("id"))
          d.moreDetails = s("details")
          d
        }
        case _ => o.toString
      } else o.toString
    }

    // parse a DieselTrace
    def trace (o:Map[String, Any]) : DieselTrace = {
      DieselTrace (
        fromj (o("root").asInstanceOf[Map[String,Any]]).asInstanceOf[DomAst],
        o.getOrElse("node", "").toString,
        o.getOrElse("engineId", "").toString,
        o.getOrElse("app", "").toString,
        o.getOrElse("details", "").toString,
        o.get("parentNodeId").map(_.toString)
      )
    }
  }

  // an instance at runtime
  case class EInstance(cls: String, attrs: List[RDOM.P]) {
  }

  // an instance at runtime
  case class EEvent(e: EInstance, met: String, attrs: List[RDOM.P]) {
  }

  // a context - MAP, use this to test the speed of MAP
  class ECtxM() {
    val attrs = new mutable.HashMap[String, P]()

    def apply(name: String): String = get(name).getOrElse("")

    def get(name: String): Option[String] = attrs.get(name).map(_.dflt)

    def put(p: P) = attrs.put(p.name, p)

    def putAll(p: List[P]) = p.foreach(x => attrs.put(x.name, x))
  }


  // find the spec of the generated message, to ref
  def spec(m:EMsg)(implicit ctx: ECtx) : Option[EMsg] = spec(m.entity, m.met)

  // find the spec of the generated message, to ref
  def spec(entity:String, met:String)(implicit ctx: ECtx) : Option[EMsg] =
    ctx.domain.flatMap(_.moreElements.collect {
      case x: EMsg
        if
        ("*" == x.entity || x.entity == entity || regexm(x.entity, entity)) &&
        ("*" == x.met || x.met == met || regexm(x.met, met))
         => x
    }.headOption)

  /** a REST request or response: content and type */
  class EEContent (val body:String, val ctype:String, val iroot:Option[Snakk.Wrapper[_]] = None) {
    def isXml = "application/xml" == ctype
    def isJson = "application/json" == ctype

    import razie.Snakk._

    /** xp root for either xml or json body */
    lazy val root:Snakk.Wrapper[_] = iroot getOrElse {
      ctype match {
        case "application/xml" => Snakk.xml(body)
        case "application/json" => Snakk.json(body)
        case x@_ => Snakk.json("")
        //throw new IllegalStateException ("unknown content-type: "+x)
      }
    }

    lazy val hasValues = root \ "values"
    lazy val r = if(hasValues.size > 0) hasValues else root

    // todo not optimal
    def exists(f: scala.Function1[P, scala.Boolean]): scala.Boolean = {
      val x = (r \ "*").nodes collect {
         case j : JsonOWrapper => {
           razie.MOLD(j.j.keys).map(_.toString).map {n=>
             P(n, r \ "*" \@@ n)
           }
        }
      }
      x.flatten.exists(f)
    }

    // todo don't like this
    def get(name:String) : Option[String]  = {
      (r \@@ name).toOption
    }

    /** name, default, expr */
    def ex(n:String, d:String, e:String)  = {
      if (e.isEmpty)
        (n, (r \@@ n OR d).toString)
      else if (e.startsWith("/") && e.endsWith("/")) // regex
        (n, e.substring(1, e.length - 1).r.findFirstIn(body).mkString)
      else
        (n, (r \@@ e OR d).toString)
    }

    /** extract the values and expressions from a response
      *
      * @param temp a list of name/expression
      * @param spec a list of name/default/expression
      * @param regex an optional regex with named groups
      */
    def extract (temp:Map[String,String], spec:Seq[(String,String, String)], regex:Option[String]) = {

      if(regex.isDefined) {
        val rex =
          if (regex.get.startsWith("/") && regex.get.endsWith("/"))
            regex.get.substring(1, regex.get.length - 1)
          else regex.get

        val jrex = Pattern.compile(rex).matcher(body)
        //          val hasit = jrex.find()
        if(jrex.find())
          (
            for(g <- spec)
              yield (
                g._1,
                Try {
                  jrex.group(g._1)
                }.getOrElse(g._2)
              )
            ).toList
        else Nil
      } else if(temp.nonEmpty) {
        val strs = temp.map { t =>
          ex(t._1, "", t._2)
        }
        strs.toList
      } else if(spec.nonEmpty) {
        // main case
        spec.map(t=>ex(t._1, t._2, t._3)).toList
      } else {
        // last ditch attempt to discover some values

        //          (
        //            if(ret.nonEmpty) ret else List(new P("result", ""))
        //            ).map(p => p.copy(dflt = res \ "values" \@@ p.name)).map(x => EVal(x))
        val res = jsonParsed(body)
        val a = res.getJSONObject("values")
        val strs = if(a.names == null) List.empty else (
          for (k <- 0 until a.names.length())
            yield (a.names.get(k).toString, a.getString(a.names.get(k).toString))
          ).toList
        strs.toList
      }
    }

  }

  // to collect msg def
  case class PCol(p:P, pos:Option[EPos])
  case class PMCol(pm:PM, pos:Option[EPos])
  case class MsgCol(m:EMsg,
               in  : ListBuffer[PCol] = new ListBuffer[PCol],
               out : ListBuffer[PCol] = new ListBuffer[PCol],
               cons: ListBuffer[PMCol] = new ListBuffer[PMCol]
               ) {
    def e = m.entity
    def a = m.met
    def toHtml = EMsg("", e, a ,Nil).withPos(m.pos).toHtmlInPage // no parms
  }

  def summarize(d: RDomain) = {
    val msgs  = new ListBuffer[MsgCol]
    val attrs = new ListBuffer[PCol]

    // todo collapse defs rather than select
    def collectMsg(n:EMsg, out:List[P] = Nil) = {
      if (!msgs.exists(x=> x.e == n.entity && x.a == n.met)) {
        msgs.append(new MsgCol(
          n,
          new ListBuffer[PCol]() ++= n.attrs.map(p=>PCol(p, n.pos)),
          new ListBuffer[PCol]() ++= (n.ret ::: out).map(p=>PCol(p, n.pos))
        ))
      } else msgs.find(x=> x.e == n.entity && x.a == n.met).foreach {m=>
        n.attrs.foreach {p=>
          if(m.in.exists(_.p.name == p.name)) {
            // todo collect types etc
          } else {
            m.in append PCol(p, n.pos)
          }
        }
      }
    }

//    def collectP(l:List[P]) = {
//      l.map(p=>collect(p.name, "", "attr"))
//    }
//    def collectPM(l:List[PM]) = {
//      l.map(p=>collect(p.name, "", "attr"))
//    }

        d.moreElements.collect({
          case n: EMsg => {
            if (!msgs.exists(x=> x.e == n.entity && x.a == n.met))
              collectMsg(n)
          }
          case n: ERule => {
            collectMsg(n.e.asMsg.withPos(n.pos))
            n.i.map {x=>
              collectMsg(x.asMsg.withPos(n.pos))
            }
          }
          case n: EMock => {
            collectMsg(n.rule.e.asMsg.withPos(n.pos), n.rule.i.flatMap(_.attrs))
          }
          case n: ExpectM => collectMsg(n.m.asMsg.withPos(n.pos))
        })

//        d.moreElements.collect({
//          case n: EMsg => collectP(n.attrs)
//          case n: ERule => {
//            collectPM(n.e.attrs)
//            collectP(n.i.attrs)
//          }
//          case n: EMock => collectPM(n.rule.e.attrs)
//          case n: ExpectM => collectPM(n.m.attrs)
//        })
  msgs
  }

  /** simple json for content assist */
  def toCAjmap(d: RDomain) = {
    val visited = new ListBuffer[(String, String, String)]()

    // todo collapse defs rather than select
    def collect(e: String, m: String, kind:String="msg") = {
      if (!visited.exists(_ ==(kind, e, m))) {
        visited.append((kind, e, m))
      }
    }

    def collectP(l:List[P]) = {
      l.map(p=>collect(p.name, "", "attr"))
    }
    def collectPM(l:List[PM]) = {
      l.map(p=>collect(p.name, "", "attr"))
    }

    // add known executors
    Executors.all.flatMap(_.messages).map(m=> collect(m.entity, m.met))

    Map(
      "msg" -> {
        d.moreElements.collect({
          case n: EMsg => collect(n.entity, n.met)
          case n: ERule => {
            collect(n.e.cls, n.e.met)
            n.i.map {x=>
              collect(x.cls, x.met)
            }
          }
          case n: EMock => collect(n.rule.e.cls, n.rule.e.met)
          case n: ExpectM => collect(n.m.cls, n.m.met)
        })
        visited.filter(_._1 == "msg").toList.map(t => t._2 + "." + t._3)
      },
      "attr" -> {
        d.moreElements.collect({
          case n: EMsg => collectP(n.attrs)
          case n: ERule => {
            collectPM(n.e.attrs)
            n.i.map { x =>
              collectP(x.attrs)
            }
          }
          case n: EMock => collectPM(n.rule.e.attrs)
          case n: ExpectM => collectPM(n.m.attrs)
        })

        // return collected
        visited.filter(_._1 == "attr").toList.map(t => t._2)
      }
    )
  }

  /** nice links to stories in AST trees */
  case class StoryNode (path:TSpecPath) extends CanHtml with InfoNode {
    override def toHtml = "Story " + path.ahref
    override def toString = "Story " + path.wpath
  }

  /* add a message */
  def addMsgToAst(root: DomAst, v : EMsg) = {
    root.children append DomAst(v, AstKinds.RECEIVED)
  }

  /* extract more nodes to run from the story - add them to root */
  def addStoryToAst(root: DomAst, stories: List[DSpec], justTests: Boolean = false, justMocks: Boolean = false, addFiddles:Boolean=false) = {
    var lastMsg: Option[EMsg] = None
    var lastMsgAst: Option[DomAst] = None
    var lastAst: List[DomAst] = Nil
    var inSequence = true

    def addMsg(v: EMsg) = {
      lastMsg = Some(v);
      // withPrereq will cause the story messages to be ran in sequence
      lastMsgAst = if (!(justTests || justMocks)) Some(DomAst(v, AstKinds.RECEIVED).withPrereq({
        if (inSequence) lastAst.map(_.id)
        else Nil
      })) else None // need to reset it
      lastAst = lastMsgAst.toList
      lastAst
    }

    def addStory (story:DSpec) = {

      if(stories.size > 1 || addFiddles)
        root.children appendAll {
          lastAst = List(DomAst(StoryNode(story.specPath), "story").withPrereq(lastAst.map(_.id)))
          lastAst
        }

      root.children appendAll RDomain.domFilter(story) {
        case o: O if o.name != "context" => List(DomAst(o, AstKinds.RECEIVED))
        case v: EMsg if v.entity == "ctx" && v.met == "storySync" => {
          inSequence = true
          Nil
        }
        case v: EMsg if v.entity == "ctx" && v.met == "storyAsync" => {
          inSequence = false
          Nil
        }
        case v: EMsg => addMsg(v)
        case v: EVal => List(DomAst(v, AstKinds.RECEIVED))
        case v: ERule => List(DomAst(v, AstKinds.RULE))
        case v: EMock => List(DomAst(v, AstKinds.RULE))
        case e: ExpectM if (!justMocks) => {
          lastAst = List(DomAst(e.withGuard(lastMsg.map(_.asMatch)).withTarget(lastMsgAst), "test").withPrereq(lastAst.map(_.id)))
          lastAst
        }
        case e: ExpectV if (!justMocks) => {
          lastAst = List(DomAst(e.withGuard(lastMsg.map(_.asMatch)).withTarget(lastMsgAst), "test").withPrereq(lastAst.map(_.id)))
          lastAst
        }
      }.flatten
    }

    stories.foreach (addStory)
  }

  case class TestResult(value: String, more: String = "") extends CanHtml with HasPosition {
    var pos : Option[EPos] = None
    def withPos(p:Option[EPos]) = {this.pos = p; this}

    override def toHtml =
      if (value == "ok")
        span(value, "success") + s" $more"
      else if (value startsWith "fail")
        span(value, "danger") + s" $more"
      else
        span(value, "warning") + s" $more"

    override def toString =
      value + s" $more"
  }

  def label(value:String, color:String="default") =
    s"""<span class="label label-$color">$value</span>"""

}

