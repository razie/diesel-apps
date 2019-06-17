/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.diesel.exec

import jdk.nashorn.api.scripting.ScriptObjectMirror
import razie.base.scriptingx.DieselScripster
import razie.diesel.dom.RDOM._
import razie.diesel.dom._
import razie.diesel.engine.DieselJs
import razie.diesel.engine.RDExt.spec
import razie.diesel.ext.{MatchCollector, _}

// the context persistence commands
class EEFunc extends EExecutor("func") {

  // can execute even in mockMode
  override def isMock = true

  override def test(in: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    ctx.domain.exists(_.funcs.contains(in.entity + "." + in.met))
  }

  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] = {
    val res = ctx.domain.flatMap(_.funcs.get(in.entity + "." + in.met)).map { f =>
      val res = try {
        if (f.script != "") {
          val c = ctx.domain.get.mkCompiler("js")

          // compile all other functions
          val x = c.compileAll(c.not { case fx: RDOM.F if fx.name == f.name => true })

          // add this one, as an expression
          var s = x + "\n" + c.compile(f)

          // call it
          s = s + "\n" + c.callInContext(f)

          val q = in.attrs.map(t => (t.name, t.dflt)).toMap

          val r = EEFunc.newestFiddle(s, "js", in.attrs, ctx)
          r._3
        } else
          "ABSTRACT FUNC"
      } catch {
        case e: Throwable => e.getMessage
      }
      res.toString
    } getOrElse s"no func ${in.met} in domain"

    in.ret.headOption.orElse(spec(in).flatMap(_.ret.headOption)).orElse(
      Some(new P("payload", ""))
    ).map(_.copy(dflt = res)).map(x => EVal(x)).toList
  }

  override def toString = "$executor::func "
}

object EEFunc {
  def execute (script:String)(implicit ctx: ECtx): Any = {
    val res : Any = try {
      val r = newestFiddle(script, "js", ctx.listAttrs, ctx)
      r._2
    } catch {
      case e: Throwable => e.getMessage
    }

    res
  }

  /** core of JS execution */
  def executeTyped (script:String)(implicit ctx: ECtx): P = {
    val r = try {

      // run the script
      val r = newestFiddle(script, "js", ctx.listAttrs, ctx)

      // typed result
      r._3 match {

        case i:Integer => P("", i.toString, WTypes.NUMBER).withValue(i, WTypes.NUMBER)

        case i:Double => P("", i.toString, WTypes.NUMBER).withValue(i, WTypes.NUMBER)

        case o : ScriptObjectMirror => {
          P("", r._2.toString, WTypes.JSON).withValue(o, WTypes.appJson)
        }

        case e: Throwable => P("", e.getMessage, WTypes.EXCEPTION).withValue(e, WTypes.EXCEPTION)

        case _ => P("", r._2.toString, WTypes.STRING)
      }
    } catch {
      case e: Throwable => P("", e.getMessage, WTypes.EXCEPTION).withValue(e, WTypes.EXCEPTION)
    }

    r
    }

  /** this to be the new entry point for scripts in diesel context */
  def newestFiddle(script: String, lang: String, attrs: List[P], ctx:ECtx) = {
    // todo optimize - remove q
    val q  = attrs.map(t => (t.name, t.dflt)).toMap + ("diesel" -> "")
    val qp = attrs.map(t => (t.name, t)).toMap
    val exprs = Map[String,String]()

    val typed = qTypedP(qp, None)(ctx) + ("diesel" -> new DieselJs(ctx))

    val r = DieselScripster.isfiddleMap(script, "js", q, Some(typed), exprs, ctx)

    r
  }

}

