/*  ____    __    ____  ____  ____  ___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import org.bson.types.ObjectId
import razie.Logging
import razie.diesel.dom.DieselAssets
import razie.diesel.dom.RDOM.P
import razie.diesel.engine.nodes.EMsg
import razie.diesel.expr.ScopeECtx
import razie.diesel.model.DieselMsg
import razie.wiki.model.WID
import scala.collection.mutable.ListBuffer

/** simple streaming for inter-flow comms
  *
  * A stream needs only one consumer flow (the owner). When the owner dies, the stream dies too.
  * The stream has one actor that manages it - all access is via its actor.
  *
  * put elements on the stream from various flows and they'll be sent to the consumer flow.
  *
  * The stream creates new message for each batchf
  *
  * More needed: backpressure, persistence, queueing, timed slice window, distributed
  *
  * NOT - MT-safe, need to wrap in an actor
  *
  * @param correlationId is parentEngineID.parentSuspendID - if dot is missing, this was a fire/forget
  */
abstract class DomStream(
  val owner: DomEngine,
  val name: String,
  val description: String,
  val batch: Boolean = false,
  val batchSize: Int,
  val context: P = P.of("context", "{}"),
  val correlationId: Option[String] = None,
  val maxSize: Int = 100,
  val id: String = new ObjectId().toString) extends Logging {

  assert(name.trim.length > 0, "streams need unique names")

  def wid = WID("DieselStream", name)

  def href = DieselAssets.mkAhref(WID("DieselStream", this.name))

  def href(format: String = "") = s"/diesel/engine/view/$id?format=$format"

  var synchronous = false
  private var targetId: Option[String] = None // target parent for consume nodes

  /** specify target parent for the consume nodes */
  def withTargetId(id: String) = {
    this.targetId = Some(id)
    this
  }

  // the actual elements
  private val list: ListBuffer[Any] = new ListBuffer[Any]()
  private val errors: ListBuffer[P] = new ListBuffer[P]()
  private var isDone = false
  private var isError = false
  private var isConsumed = false

  def getIsConsumed = isConsumed

  def streamIsDone = isDone

  def setCtx(ast: DomAst): DomAst = {
    // todo need to exec in separate scopes...
//    ast.replaceCtx(new ScopeECtx(Nil, targetId.map(owner.n).flatMap(_.getCtx)))
    ast
  }

  /** put some elements onto the stream
    *
    * if in single element mode - sent straight to consumers
    *
    * if in batch mode, they may be batched or not
    */
  def put(l: List[Any], justConsume: Boolean = false) = {
    trace("DStream.put: " + l.mkString(","))
    if (!justConsume) {
      assert(!isDone)

      var acceptable = Math.min(maxSize - list.size, l.size)
      if (l.size > acceptable) {
        // todo throw back to sender
        assert(false, "stream overflow")
      } else {
        trace(" - DStream appended: " + l.mkString(","))
        list.appendAll(l)
      }
    }

    // todo I send right away - should batch up with a timeout

    if (isConsumed) targetId.map { tid =>

      if (batch && batchSize > 0) {

        var pickedUp = 0
        val asts = new ListBuffer[DomAst]()

        list.toList
            .grouped(batchSize)
            .toList
            .foreach { batch =>
              trace(" - DStream sending.slice: " + batch.mkString(","))
              pickedUp += batch.size

              val m = new EMsg(
                DieselMsg.STREAMS.STREAM_ONDATASLICE,
                P.of("stream", name) ::
                    P.of("data", batch) ::
                    P.of("context", context) ::
                    Nil
              ) with KeepOnlySomeSiblings

              val ast = DomAst(m)
                  .withPrereq(
                    // todo add a "parallel" setting on consume?
                    asts.map(_.id).toList
                  )

              setCtx(ast)

              asts.append(ast)
            }

        // todo each node should have its own scope or run in sequence...

        trace(s" - DStream size ${list.size} dropping: $pickedUp")
        list.remove(0, pickedUp)
        DieselAppContext ! DEAddChildren(owner.id, tid, recurse = true, -1, asts.toList,
          Some((a, e) => a.withPrereq(getDepy)))
        trace(s" - DStream list size ${list.size} is: " + list.mkString)

      } else {

        // no batch

        list.toList.map { data =>
          val ast = DomAst(new EMsg(
            DieselMsg.STREAMS.STREAM_ONDATA,
            P.of("stream", name) ::
                P.of("data", data) ::
                P.of("context", context) ::
                Nil
          ) with KeepOnlySomeSiblings)

          setCtx(ast)

          DieselAppContext ! DEAddChildren(owner.id, tid, recurse = true, -1, List(ast),
            Some((a, e) => a.withPrereq(getDepy)))
        }

        trace(" - DStream clear: ")
        list.clear()
      }
    } else {
      // no consumers - accumulate to a point
    }
  }

  def done(justConsume: Boolean = false): Unit = {
    if (!justConsume) {
      isDone = true
      if (isConsumed) {
        consume()
      }
    } else {
      val ast = DomAst(EMsg(
        DieselMsg.STREAMS.STREAM_ONDONE,
        List(
          P.of("stream", name), P.of("context", context)
        )
      ))

      setCtx(ast)

      targetId.map { tid =>
        DieselAppContext ! DEAddChildren(owner.id, tid, recurse = true, -1, List(ast),
          Some((a, e) => a.withPrereq(getDepy)))
        DieselAppContext ! DEComplete(owner.id, targetId.get, recurse = true, -1, Nil)
      }
    }
  }

  /** get list of other generated nodes */
  def getDepy: List[String] = {
    val target = targetId.map(owner.n)
    val res = target.toList.flatMap(
      _.children
          .filter(_.status != DomState.DONE)
          .filter(_.value.isInstanceOf[EMsg])
          .map(_.id))
    res
  }

  def error(l: List[P], justConsume: Boolean = false): Unit = {
    if (!justConsume) {
      isError = true
      isDone = true
      errors.appendAll(l)
      if (isConsumed) {
        consume()
      }
    } else {

      val ast = DomAst(EMsg(
        DieselMsg.STREAMS.STREAM_ONERROR,
        List(
          P.of("stream", name), P.of("context", context)
        ) ::: errors.toList
      ))

      setCtx(ast)

      targetId.map { tid =>
        DieselAppContext ! DEAddChildren(owner.id, targetId.get, recurse = true, -1, List(ast),
          Some((a, e) => a.withPrereq(getDepy)))
        DieselAppContext ! DEComplete(owner.id, targetId.get, recurse = true, -1, Nil)
      }
    }
  }

  // owner starts consuming
  def consume(): Unit = {
    isConsumed = true
    // if something already, consume it
    if (list.size > 0) put(Nil, true)
    if (isDone && !isError) done(true)
    if (isDone && isError) error(Nil, true)

    // not done - will wait for done
//    targetId.map { tid =>
//      DieselAppContext ! DEComplete(owner.id, targetId.get, recurse = true, -1, Nil)
//    }
  }

  // it's cleanup by the owner engine
  def cleanup(): Unit = {
    // make sure it's done - don't accept anymore
    isDone = true

    // cleanup
    DieselAppContext ! DESClean(name)
  }

  override def toString = s"DomStream($name, $id)"
}

class DomStreamV1(
  owner: DomEngine,
  name: String,
  description: String,
  batch: Boolean = false,
  batchSize: Int = -1,
  context: P = P.of("context", "{}"),
  correlationId: Option[String] = None) extends DomStream(owner, name, description, batch, batchSize, context,
  correlationId) {

}
