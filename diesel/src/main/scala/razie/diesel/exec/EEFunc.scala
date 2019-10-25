/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
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
import razie.diesel.expr.DieselExprException
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
      EEFunc.exec(in, f).calculatedP
    } getOrElse P("", s"no func ${in.met} in domain")

    in.ret.headOption.orElse(spec(in).flatMap(_.ret.headOption)).orElse(
      Some(res.copy(name = "payload"))
    ).map(x=> res.copy(name = x.name)).map(x => EVal(x)).toList
  }

  override def toString = "$executor::func "
}

object EEFunc {

  /** generic call a domain function */
  def exec(in:EMsg, f: F)(implicit ctx: ECtx): P = {
      val res = try {
        if (f.script != "") {
          val c = ctx.domain.get.mkCompiler("js")

          // compile all other functions
          val x = c.compileAll(c.not { case fx: RDOM.F if fx.name == f.name => true })
          val offset = x.lines.size

          // add this one, as an expression
          var s = x + "\n" + c.compile(f)

          // call it
          s = s + "\n" + c.callInContext(f)

          val q = in.attrs.map(t => (t.name, t.currentStringValue)).toMap

          val r = newestFiddle(s, "js", in.attrs, ctx)
          scriptResToTypedP(r, offset)
        } else
          P("", "ABSTRACT FUNC", WTypes.wt.EXCEPTION)
      } catch {
        case e: Throwable => P("", e.getMessage, WTypes.wt.EXCEPTION).withValue(e, WTypes.wt.EXCEPTION)
      }
    res
  }

  def execute (lang:String, script:String)(implicit ctx: ECtx): Any = {
    val res : Any = try {
      val r = newestFiddle(script, lang, ctx.listAttrs, ctx)
      r._2
    } catch {
      case e: Throwable => e.getMessage
    }

    res
  }

  /** core of JS execution */
  def scriptResToTypedP (r:(Boolean, String, Any), offset:Int)(implicit ctx: ECtx): P = {
    val x = try {

      // typed result
      r._3 match {

       case i:Integer => P("", i.toString, WTypes.wt.NUMBER).withValue(i, WTypes.wt.NUMBER)

        case i:Double => P("", i.toString, WTypes.wt.NUMBER).withValue(i, WTypes.wt.NUMBER)

        case o : ScriptObjectMirror => {
                    P("", r._2.toString, WTypes.JSON).withCachedValue(o, WTypes.Mime.appJson, r._2.toString)
//          val str = r._2.toString
//          if(str.trim.startsWith("{"))
//            P.fromTypedValue("", str, WTypes.JSON)//.withValue(o, WTypes.appJson)
//          else if(str.trim.startsWith("["))
//            P.fromTypedValue("", str, WTypes.ARRAY)//.withValue(o, WTypes.appJson)
//          else
//            P("", r._2.toString, WTypes.UNKNOWN)//.withValue(o, WTypes.appJson)
        }

        case e: Throwable => P("", "+"+offset+r._2, WTypes.wt.EXCEPTION).withValue(e, WTypes.wt.EXCEPTION)

        case _ => P("", r._2.toString, WTypes.wt.STRING)
      }
    } catch {
      case e: Throwable => P("", e.getMessage, WTypes.wt.EXCEPTION).withValue(e, WTypes.wt.EXCEPTION)
    }

    x
  }

  /** core of JS execution */
  def executeTyped (lang:String, script:String)(implicit ctx: ECtx): P = {
    val r = try {

      // run the script
      val r = newestFiddle(script, lang, ctx.listAttrs, ctx)
      scriptResToTypedP(r, 0)
    } catch {
      case e: Throwable => P("", e.getMessage, WTypes.wt.EXCEPTION).withValue(e, WTypes.wt.EXCEPTION)
    }

    r
  }

  /** this to be the new entry point for scripts in diesel context */
  def newestFiddle(script: String, lang: String, attrs: List[P], ctx:ECtx) = {
    // todo optimize - remove q
    val q  = attrs.map(t => (t.name, t.currentStringValue)).toMap + ("diesel" -> "")
    val qp = attrs.map(t => (t.name, t)).toMap
    val exprs = Map[String,String]()

    val typed = qTypedP(qp, None)(ctx) + ("diesel" -> new DieselJs(ctx))

    val r = DieselScripster.newsfiddleMap(script, lang, q, Some(typed), false, exprs, ctx)

    r
  }

}

