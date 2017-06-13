/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package mod.diesel.model.exec

import razie.clog
import razie.diesel.dom.RDOM._
import razie.diesel.dom._
import razie.diesel.engine.{DEStartTimer, DieselAppContext, DomEngECtx}
import razie.diesel.ext.{MatchCollector, _}

import scala.collection.mutable

class EECtx extends EExecutor("ctx") {

  /** map of active contexts per transaction */
  val contexts = new mutable.HashMap[String, ECtx]()

  override def isMock: Boolean = true

  override def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    m.entity == "ctx"
  }

  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] = {
    def parm(s: String): Option[P] = in.attrs.find(_.name == s).orElse(ctx.getp(s))

    in.met match {
      case "persisted" => {
        contexts.get(ctx("kind") + ctx("id")).map(x =>
          if (ctx != x)
            ctx.root.asInstanceOf[DomEngECtx].overwrite(x)
        ).getOrElse {
          contexts.put(ctx("kind") + ctx("id"), ctx.root) // should I save this one?
        }
        Nil
      }
      case "log" => {
        clog << "DIESEL.log " + ctx.toString
        Nil
      }
      case "test" => {
        clog << "DIESEL.test " + ctx.toString

        Nil
      }
      case "clear" => {
        //          contexts.put(ctx("kind") + ctx("id"), ctx.root) // should I save this one?
        Nil
      }
      case "engineSync" => {
        // turn the engine sync
        ctx.root.asInstanceOf[DomEngECtx].engine.map(_.synchronous = true)
        Nil
      }
      case "reset" => {
        //          ctx.root.asInstanceOf[DomEngECtx].reset
        Nil
      }
      case "foreach" => {
        val list = ctx.getp(parm("list").get.dflt).get
        val RE = """(\w+)\.(\w+)""".r
        val RE(e, m) = parm("msg").get.dflt

        razie.js.parse(s"{ list : ${list.dflt} }").apply("list") match {
          case l: List[Any] => {
            val nat = in.attrs.filter(e => !Array("list", "item", "msg").contains(e.name))
            l.map { item: Any =>
              val is = razie.js.anytojsons(item)
              EMsg("", e, m, RDOM.typified(parm("item").get.dflt, is) :: nat)
            }
          }
          case x@_ => {
            List(EError("list was not a list", x.getClass.getName))
          }
        }
      }
      case "echo" => {
        in.attrs.map { a =>
          if (a.dflt != "")
            new EVal(a.name, a.dflt) // todo calc exprs
          else
            new EVal(a.name, ctx(a.name)) // if not given, then find it
        }
      }
      case "sleep" => {
        val d = in.attrs.find(_.name == "duration").map(_.dflt.toInt).getOrElse(1000)
        Thread.sleep(d)
        new EInfo("ctx.sleep - slept " + d) :: Nil
      }
      case "timer" => {
        val d = in.attrs.find(_.name == "duration").map(_.dflt.toInt).getOrElse(1000)
        val m = in.attrs.find(_.name == "msg").map(_.dflt).getOrElse("$msg ctx.echo (msg=\"timer without message\")")
        DieselAppContext.router.map(_ ! DEStartTimer("x", d, Nil))
        new EInfo("ctx.timer - start " + d) :: Nil
      }
      case s@_ => {
        new EError(s"ctx.$s - unknown activity ") :: Nil
      }
    }
  }

  override def toString = "$executor::ctx "

  override val messages: List[EMsg] =
    EMsg("", "ctx", "persisted", Nil) ::
      EMsg("", "ctx", "log", Nil) ::
      EMsg("", "ctx", "echo", Nil) ::
      EMsg("", "ctx", "test", Nil) ::
      EMsg("", "ctx", "engineSync", Nil) ::
      EMsg("", "ctx", "storySync", Nil) :: // processed by the story teller
      EMsg("", "ctx", "storyAsync", Nil) :: // processed by the story teller
      EMsg("", "ctx", "clear", Nil) :: Nil
}
