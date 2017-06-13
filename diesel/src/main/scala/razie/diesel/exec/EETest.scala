/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.diesel.exec

import razie.diesel.dom._
import razie.diesel.ext.{MatchCollector, _}

// execute tests
class EETest extends EExecutor("test") {
  override def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    m.stype.startsWith("TEST.")
  }

  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] = {
    in.ret.headOption.map(_.copy(dflt = in.stype.replaceFirst("TEST.", ""))).map(EVal).map(_.withPos(in.pos)).toList
  }

  override def toString = "$executor::test "
}
