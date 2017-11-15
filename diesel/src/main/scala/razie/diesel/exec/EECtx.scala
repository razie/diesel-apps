/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package mod.diesel.model.exec

import org.apache.commons.codec.digest.DigestUtils
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
              EMsg(e, m, RDOM.typified(parm("item").get.dflt, is) :: nat)
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

      case "set" => {
        val n = in.attrs.find(_.name == "name").map(_.dflt)
        val v = in.attrs.find(_.name == "value")
        n.flatMap { name =>
          if (v.exists(_.dflt != ""))
            Some(new EVal(name, v.get.dflt))
          else if (v.exists(_.expr.isDefined))
            Some(new EVal(typified(name, v.get.expr.get.apply(""))))
          else {
            // clear it
            ctx.remove(name)
            None
          }
        }.toList
      }

      case "setAll" => {
        // set all parms passed in
        in.attrs.map { p =>
          if (p.dflt != "")
            Some(new EVal(p.name, p.dflt))
          else if (p.expr.isDefined)
            Some(new EVal(typified(p.name, p.expr.get.apply(""))))
          else {
            // clear it
            ctx.remove(p.name)
            None
          }
        }.filter(_.isDefined).map(_.get)
      }

      case "sleep" => {
        val d = in.attrs.find(_.name == "duration").map(_.dflt.toInt).getOrElse(1000)
        Thread.sleep(d)
        new EInfo("ctx.sleep - slept " + d) :: Nil
      }

      case "sha1" => {
        val res = in.attrs.filter(_.name != "name").map { a =>
          val md = java.security.MessageDigest.getInstance("SHA-1")
          val s = md.digest(a.dflt.getBytes("UTF-8")).map("%02X".format(_)).mkString
//          val sb = DigestUtils.sha1Hex(a.dflt)
          new EVal(a.name+"_sha1", s) //:: new EVal(a.name+"_sha1j", sb) :: Nil
        }

        res :::
          in.attrs
            .find(_.name=="name")
            .map(_.calculateValue)
            .map(p=> new EVal(p, res.head.p.dflt))
            .toList

//        new EVal(a.name+"_sha1", s) :: new EVal("result", s) :: Nil
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
    EMsg("ctx", "persisted") ::
      EMsg("ctx", "log") ::
      EMsg("ctx", "echo") ::
      EMsg("ctx", "test") ::
      EMsg("ctx", "engineSync") ::
      EMsg("ctx", "storySync") :: // processed by the story teller
      EMsg("ctx", "storyAsync") :: // processed by the story teller
      EMsg("ctx", "clear") :: Nil
}
