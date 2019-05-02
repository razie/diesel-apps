/**
 *  ____    __    ____  ____  ____  ___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import org.bson.types.ObjectId
import org.joda.time.DateTime
import razie.clog
import razie.diesel.dom.RDOM.P
import razie.diesel.dom.{DomState, RDomain, _}
import razie.diesel.engine.RDExt._
import razie.diesel.ext.{BFlowExpr, FlowExpr, MsgExpr, SeqExpr, _}
import razie.diesel.utils.DomCollector
import razie.tconf.DSpec
import razie.wiki.Enc

import scala.Option.option2Iterable
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.Try

/** a trace */
case class DieselTrace (
  root:DomAst,
  node:String,        // actual server node
  engineId:String,      // engine Id
  app:String,         // application/system id twitter:pc:v5
  details:String="",
  parentNodeId:Option[String]=None ) extends CanHtml with InfoNode {

  def toj: Map[String, Any] =
    Map(
      "class" -> "DieselTrace",
      "ver" -> "v1",
      "node" -> node,
      "engineId" -> engineId,
      "app" -> app,
      "details" -> details,
      "id" -> root.id,
      "parentNodeId" -> parentNodeId.mkString,
      "root" -> root.toj
    )

  def toJson = toj

  override def toHtml =
    span("trace::", "primary") +
      s"$details (node=$node, engine=$engineId, app=$app) :: " //+ root.toHtml

  override def toString = toHtml

  def toAst  = {
    val me = new DomAst (this, AstKinds.SUBTRACE)
    me.children.append(root)
    me
  }
}


