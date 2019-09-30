/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.diesel.exec

import mod.diesel.model.exec.EESnakk
import razie.clog
import razie.diesel.Diesel
import razie.diesel.dom.RDOM._
import razie.diesel.dom._
import razie.diesel.engine.InfoAccumulator
import razie.diesel.engine.RDExt.spec
import razie.diesel.ext.{MatchCollector, _}
import razie.wiki.Enc

/** format template results: if the current message has a template for a "result" then apply it.
  *
  * If a message has a "payload" type template, this is it's executor.
  *
  * this is just a simple use of templates - equivalent to setting payload to a templatized value
  *
  * todo simpler mechanism, something like:
  * (payload = """tempalte ${ss}""" as Json) but that would not be recognized as a template by tspecs
  */
class EEFormatter extends EExecutor("formatter") {

  /** can I execute this task? */
  override def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    // is there a "result" type template?
    spec(m).exists(m => ctx.findTemplate(m.ea).exists(x=>
      x.tags.contains(Diesel.PAYLOAD)
    ))
  }

  /** execute the task then */
  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] = {
    val templateReq  = ctx.findTemplate(in.ea, Diesel.PAYLOAD)

    // expanded template content
    val formattedTemplate = templateReq.map(_.content).map { content =>
      EESnakk.prepStr2(content, in.attrs)
    }

    formattedTemplate.map {res=>
      var eres = new InfoAccumulator()

      clog << ("PAYLOAD: \n" + res)
      eres += EInfo("Payload", Enc.escapeHtml(res))

      // todo respect template mimetypes
      val pos = (templateReq.map(_.pos)).orElse(spec(in).flatMap(_.pos))
      ctx.put(new P(Diesel.PAYLOAD, res))
      eres += new EVal(Diesel.PAYLOAD, res).withPos(pos)

      eres.eres
    } getOrElse
      EError("No formatting template found for " + in.toString) :: Nil
  }

  override def toString = "$executor::formatter "
}


