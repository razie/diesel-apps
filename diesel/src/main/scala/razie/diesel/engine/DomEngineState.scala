/*  ____    __    ____  ____  ____  ___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import org.joda.time.DateTime
import razie.diesel.engine.nodes.CanHtml
import razie.diesel.expr._
import scala.collection.mutable.ListBuffer

/** DDD - base trait for events */
trait DEvent {
  def dtm: DateTime
}

/** DDD - node expanded */
case class DEventAddStream(stream: DomStream, dtm: DateTime = DateTime.now) extends DEvent with
    CanHtml {
  override def toHtml = s"EvAddStream: ${stream.name}"
}

/** DDD - node expanded */
case class DEventExpNode(nodeId: String, children: List[DomAst], dtm: DateTime = DateTime.now) extends DEvent with
    CanHtml {
  override def toHtml = s"EvExpNode: $nodeId, ${children.map(_.toString).mkString}"
}

/** DDD - node status */
case class DEventNodeStatus(nodeId: String, status: String, dtm: DateTime = DateTime.now) extends DEvent with CanHtml {
  override def toHtml = s"EvNodeStatus: $nodeId, $status}"
}

/** dependency between two nodes */
case class DADepy (prereq:DomAst, depy:DomAst)

/** dependency event */
case class DADepyEv (prereq:String, depy:String, dtm:DateTime=DateTime.now) extends DEvent


/** the state changes of an engine, including all events, tree changes etc, go through here */
trait DomEngineState {

  var maxLevels = 45
  var maxExpands = 10000
  var curExpands = 0

  var status = DomState.INIT

  var seqNo = 0 // each node being processed in sequence, gets a stamp here, so you can debug what runs in parallel
  def seq() = {
    val t = seqNo
    seqNo = seqNo + 1
    t
  }

  var ownedStreams: ListBuffer[DomStream] = new ListBuffer[DomStream]

  // we need settings
  def settings: DomEngineSettings

  // setup the context for this eval
  implicit def ctx: ECtx

  def n(id: String): DomAst

  /** dependencies */
  protected val depys = ListBuffer[DADepy]()

  /** create / add some dependencies, so d waits for p */
  protected def crdep(p:List[DomAst], d:List[DomAst]) = {
    d.map{d=>
      // d will wait
      if (p.nonEmpty) evChangeStatus(d, DomState.DEPENDENT)

      p.map { p =>
        evAddDepy(p, d)
      }
    }
  }

  //========================== DDD
  /**
    * we keep a list of events with all the changes to the current engine (nodes status, parameters, new nodes etc)
    *
    * this is/will be used for persisting/recovering engines, distributed hot-hot etc
    */

  val events: ListBuffer[DEvent] = new ListBuffer[DEvent]()

  def addEvent(e: DEvent*): Unit = {
    // collect events if needed
    if (!settings.slaSet.contains(DieselSLASettings.NOPERSIST)) {
      events.append(e: _*)
    }

    //
    e collect {

      case DEventAddStream(stream, _) => {
        ownedStreams.append(stream)

        this.curExpands = curExpands + 1
      }

      case DEventExpNode(parentId, children, _) => {
        // must add directly to children, to avoid recursing
        val parent = n(parentId)
        parent.appendAllNoEvents(children)

        this.curExpands = curExpands + 1
      }

      case DEventNodeStatus(parentId, status, _) =>
        n(parentId).status = status

      case DADepyEv(pId, dId, _) =>
        depys.append(DADepy(n(pId), n(dId)))
    }
  }

  // todo it's faster here where i have the node handles than looking it up by id above...

  def evAppStream(stream: DomStream): Unit =
    addEvent(DEventAddStream(stream))

  def evAppChildren(parent: DomAst, children: DomAst): Unit =
    evAppChildren(parent, List(children))

  def evAppChildren(parent: DomAst, children: List[DomAst]): Unit = {
    addEvent(DEventExpNode(parent.id, children))
  }

  private[engine] def evChangeStatus(node: DomAst, status: String): Unit = {
//    node.status = status
    addEvent(DEventNodeStatus(node.id, status))
  }

  private[engine] def evAddDepy (p:DomAst, d:DomAst) : Unit = {
//    depys.append(DADepy(p,d))
    addEvent(DADepyEv(p.id,d.id))
  }

}

/** use this when building ASTs */
object NoEngineState extends DomEngineState {

//   setup the context for this eval
  implicit def ctx : ECtx = null

  def n(id:String):DomAst = ???

  override def addEvent(e:DEvent*) : Unit = {}

  override def evAppChildren (parent:DomAst, children:DomAst) : Unit = {}

  override def evAppChildren (parent:DomAst, children:List[DomAst]) : Unit = {}

  private[engine] override def evChangeStatus (node:DomAst, status:String) : Unit = {}

  private[engine] override def evAddDepy (p:DomAst, d:DomAst) : Unit = {}

  override def settings = ???
}

