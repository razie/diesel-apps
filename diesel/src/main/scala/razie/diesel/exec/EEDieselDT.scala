/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.diesel.exec

import razie.diesel.dom._
import razie.diesel.ext.{MatchCollector, _}


/** actual share table. Collection model:
  * coll
  * */
class EEDieselDT extends EExecutor("diesel.dt") {
  final val DT = "diesel.dt"

  override def isMock: Boolean = true

  override def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    m.entity == DT
  }

  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] = synchronized {

    in.met match {

      case "ajson" => {
        val p = ctx.getRequiredp("payload").calculatedTypedValue
        val j = p.asJson

        List(
//          EVal(P.fromTypedValue("id", id.toString)),
//          EVal(P.fromTypedValue("payload", id.toString))
        )
      }

      case s@_ => {
        new EError(s"$s - unknown activity ") :: Nil
      }
    }
  }

  override def toString = "$executor::dt "

  override val messages: List[EMsg] =
    EMsg(DT, "ojson") ::
        EMsg(DT, "ajson") :: Nil
}

