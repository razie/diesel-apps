/*  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine.exec

import razie.diesel.dom.RDOM.P
import razie.diesel.dom.WTypes
import razie.diesel.engine._
import razie.diesel.engine.nodes._
import razie.diesel.expr.ECtx

object EEStreams {
  final val PREFIX = "diesel.stream"
}

/** executor for "ctx." messages - operations on the current context */
class EEStreams extends EExecutor(EEStreams.PREFIX) {

  import razie.diesel.engine.exec.EEStreams.PREFIX

  override def isMock: Boolean = true

  override def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    m.entity == PREFIX && messages.exists(_.met == m.met)
    // don't eat .onDone...
  }

  /**
    * this is client-side: these opertaions go through the stream's actor
    */
  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] = {
    in.met match {

      case "new" => {
        val name = ctx.getRequired("stream")
        val batch = ctx.getp("batch").map(_.calculatedTypedValue.asBoolean).getOrElse(false)
        val batchSize = ctx.getp("batchSize").map(_.calculatedTypedValue.asInt).getOrElse(100)

        // todo factory for V1
        val s = DieselAppContext.mkStream(new DomStreamV1(ctx.root.engine.get, name, name, batch, batchSize))
        ctx.root.engine.get.evAppStream(s)

        EInfo("stream - creating " + name) ::
            EVal(P.fromTypedValue(name, s, WTypes.wt.OBJECT)) ::
            Nil
      }

      case "put" => {
        val name = ctx.getRequired("stream")
        val parms = in.attrs.filter(_.name != "stream").map(_.calculatedP)
        val list = parms.map(_.calculatedTypedValue.value)
        DieselAppContext ! DESPut(name, list)
        EInfo(s"stream.put - put ${list.size} elements") :: Nil
      }

      case "putAll" => {
        val name = ctx.getRequired("stream")
        val parms = in.attrs.filter(_.name != "stream").map(_.calculatedP)
        val list = parms.flatMap(_.calculatedTypedValue.asArray.toList)
        DieselAppContext ! DESPut(name, list)
        EInfo(s"stream.put - put ${list.size} elements") :: Nil
      }

      case "error" => {
        val name = ctx.getRequired("stream")
        val parms = in.attrs.filter(_.name != "stream").map(_.calculatedP)

        DieselAppContext ! DESError(name, parms)

        EInfo(s"stream.done") :: Nil
      }

      case "done" => {
        val name = ctx.getRequired("stream")
        DieselAppContext ! DESDone(name)

        EInfo(s"stream.done") :: Nil
      }

      case "consume" => {
        val name = ctx.getRequired("stream")
        val timeout = ctx.get("timeout")

        if (DieselAppContext.activeStreamsByName.get(name).isDefined) {
          EEngSuspend("stream.consume", "", Some((e, a, l) => {
            val stream = DieselAppContext.activeStreamsByName.get(name).get
            stream.withTargetId(a.id)

            DieselAppContext ! DESConsume(stream.name)

            timeout.foreach(d => {
              DieselAppContext ! DELater(e.id, d.toInt, DEComplete(e.id, a.id, recurse = true, l, Nil))
            })
          })) ::
              Nil
        } else {
          EError(s"stream.consume - stream not found: " + name) :: Nil
        }
      }

      case s@_ => {
        new EError(s"ctx.$s - unknown activity ") :: Nil
      }
    }
  }

  override def toString = "$executor::ctx "

  override val messages: List[EMsg] =
    EMsg(PREFIX, "put") ::
        EMsg(PREFIX, "new") ::
        EMsg(PREFIX, "done") ::
        EMsg(PREFIX, "consume") ::
        EMsg(PREFIX, "putAll") ::
        Nil
}
