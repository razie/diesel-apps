/*  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine.nodes

import razie.diesel.engine.DomAst
import razie.diesel.engine.exec.EApplicable
import razie.diesel.expr.ECtx
import razie.tconf.EPos

/** flows seq/par and depys are processed directly by the engine */
case class EFlow(e: EMatch, ex: FlowExpr) extends CanHtml with EApplicable with HasPosition {
  var pos: Option[EPos] = None

  override def test(ast: DomAst, m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) =
    e.test(ast, m, cole)

  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] =
    Nil

  override def toHtml = span("$flow::") + s" ${e.toHtml} => $ex <br>"

  override def toString = s"$$flow:: $e => $ex"
}

/** base class for flow expressions */
class FlowExpr()

/** either sequence or parallel */
case class SeqExpr(op: String, l: Seq[FlowExpr]) extends FlowExpr {
  override def toString = l.mkString(op)
}

/** a single message in a flow */
case class MsgExpr(ea: String) extends FlowExpr {
  override def toString = ea
}

/** a block of a flow expr, i.e. in brackets */
case class BFlowExpr(b: FlowExpr) extends FlowExpr {
  override def toString = s"( $b )"
}


