/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.diesel.exec

import mod.diesel.model.exec.EESnakk
import razie.clog
import razie.diesel.dom.RDOM._
import razie.diesel.dom._
import razie.diesel.engine.InfoAccumulator
import razie.diesel.engine.RDExt.spec
import razie.diesel.ext.{MatchCollector, _}
import razie.wiki.Enc
import razie.wiki.parser.SimpleExprParser

/** format template results */
class EEFormatter extends EExecutor("formatter") {

  /** can I execute this task? */
  override def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    // is there a "result" type template?
    spec(m).exists(m => ctx.findTemplate(m.entity + "." + m.met).exists(x=>
      x.parmStr.startsWith("result")
    ))
  }

  /** execute the task then */
  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] = {
    val templateReq  = ctx.findTemplate(in.entity + "." + in.met, "result")

    // expanded template content
    val formattedTemplate = templateReq.map(_.content).map { content =>
      EESnakk.prepStr2(content, in.attrs)
    }

    formattedTemplate.map {res=>
      var eres = new InfoAccumulator()

      clog << ("RESULT: \n" + res)
      eres += EInfo("Result", Enc.escapeHtml(res))

      val pos = (templateReq.map(_.pos)).orElse(spec(in).flatMap(_.pos))
      ctx.put(new P("result", res))
      eres += new EVal("result", res).withPos(pos)

      eres.eres
    } getOrElse
      EError("No formatting template found for " + in.toString) :: Nil
  }

  override def toString = "$executor::formatter "
}


