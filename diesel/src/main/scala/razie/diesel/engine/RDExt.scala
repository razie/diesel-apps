/**
 *  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import mod.diesel.model.exec._
import razie.Logging
import razie.diesel.dom.RDOM._
import razie.diesel.dom.{RDomain, _}
import razie.diesel.exec.{EEDieselDT, EEDieselMongodDb, EEDieselSharedDb, EEFormatter, EEFunc, EETest}
import razie.diesel.ext.{CanHtml, _}
import razie.tconf.{DSpec, EPos, TSpecPath}
import razie.wiki.Enc
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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
object RDExt extends Logging {

  def init = {
    new EECtx ::
      new EEDieselDT ::
      new EEDieselMemDb ::
      new EEDieselSharedDb ::
      new EEDieselMongodDb ::
      new EESnakk ::
      new EETest ::
      new EEFunc ::
      new EEFormatter ::
      Nil map Executors.add
  }

  /** parse from/to json utils */
  object DieselJsonFactory {
    private def sm(x:String, m:Map[String, Any]) : String =
      if(m.contains(x)) m(x).asInstanceOf[String] else ""

    def fromj (o:Map[String, Any]) : Any = {
      def s(x:String)  : String = sm(x, o)
      def l(x:String) = if(o.contains(x)) o(x).asInstanceOf[List[_]] else Nil

      def parms (name:String) = l(name).collect {
        case m:Map[_, _] => P(
          sm("name",m.asInstanceOf[Map[String, Any]]),
          sm("value",m.asInstanceOf[Map[String, Any]])
        )
      }

      if(o.contains("class")) s("class") match {
        case "EMsg" => EMsg(
          s("entity"),
          s("met"),
          parms("attrs"),
          s("arch"),
          parms("ret"),
          s("stype")
        ).withPos(if(o.contains("pos")) Some(new EPos(o("pos").asInstanceOf[Map[String, Any]])) else None)

        case "EVal" => EVal (P(
          s("name"), s("value")
        )).withPos(if(o.contains("pos")) Some(new EPos(o("pos").asInstanceOf[Map[String, Any]])) else None)

        case "DomAst" => {
          val c = l("children").collect {
            case m:Map[_, _] => fromj(m.asInstanceOf[Map[String, Any]]).asInstanceOf[DomAst]
          }

          val v = o("value") match {
            case s:String => s
            case m : Map[_,_] => DieselJsonFactory.fromj(m.asInstanceOf[Map[String, Any]])
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

    def get(name: String): Option[String] = attrs.get(name).map(_.currentStringValue)

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
    def toHtml = EMsg(e, a).withPos(m.pos).toHtmlInPage // no parms
  }

  /** create a summary of all relevant entities in teh domain: messages and attributes */
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
                x match {
                  case e:EMapCls => collectMsg(e.asMsg.withPos(n.pos))
                }
            }
          }
          case n: EMock => {
            collectMsg(n.rule.e.asMsg.withPos(n.pos), n.rule.i.collect{
              case e:EMapCls => e
            }.flatMap(_.attrs))
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
    Executors.withAll(_.flatMap(_.messages).map(m=> collect(m.entity, m.met)))

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
            n.i.collect{
              case e:EMapCls => e
            }.map { x =>
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

  /** record a test result
    *
    * @param value "ok" for good, otherwise is failed
    * @param more - an escaped value
    * @param moreHtml - unescaped html
    */
  case class TestResult(value: String, more: String = "", moreHtml:String="") extends CanHtml with HasPosition {
    var pos : Option[EPos] = None
    def withPos(p:Option[EPos]) = {this.pos = p; this}

    override def toHtml = {
      def htrimmedDflt =
        if(more.size > 80) more.take(60)
        else more

      val hmore = Enc.escapeHtml(htrimmedDflt) + " " + moreHtml

      if (value == "ok")
        kspan(value, "success") + s" $hmore"
      else if (value startsWith "fail")
        kspan(value, "danger", Some(EPos.EMPTY), None, Some("error")) + s" $hmore"
      else
        kspan(value, "warning") + s" $hmore"
    }

    override def toString =
      value + s" $more"
  }

  def label(value:String, color:String="default") =
    s"""<span class="label label-$color">$value</span>"""

}

