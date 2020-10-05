/*  ____    __    ____  ____  ____  ___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import controllers.DieselAssets
import org.bson.types.ObjectId
import razie.Logging
import razie.diesel.dom.RDOM.P
import razie.diesel.engine.nodes.EMsg
import razie.diesel.model.DieselMsg
import razie.wiki.model.WID
import scala.collection.mutable.ListBuffer

/** the engine: one flow = one engine = one actor
  *
  * this class is a generic engine, managing the execution of the nodes, starting from the root
  *
  * specific instances, like DomEngineV1 will take care of the actual execution of nodes. The two methods to
  * implement are:
  * - expand
  *
  *
  * The DomEngineExec is the actual implementation
  *
  * @param correlationId is parentEngineID.parentSuspendID - if dot is missing, this was a fire/forget
  *
  */
abstract class DomStream(
  val owner: DomEngine,
  val name: String,
  val description: String,
  val batch: Boolean = false,
  val batchSize: Int,
  val correlationId: Option[String] = None,
  val id: String = new ObjectId().toString) extends Logging {

  assert(name.trim.length > 0, "streams need unique names")

  def wid = WID("DieselStream", name)

  def href = DieselAssets.mkAhref(WID("DieselStream", this.name))

  def href(format: String = "") = s"/diesel/engine/view/$id?format=$format"

  var synchronous = false
  private var targetId: Option[String] = None

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

  def put(l: List[Any], justConsume: Boolean = false) = {
    if (!justConsume) {
      assert(!isDone)
      list.appendAll(l)
    }

    targetId.map { tid =>
      if (batch && batchSize > 0) {
        var pickedUp = 0
        val x = list.toList
            .grouped(batchSize)
            .toList
        list.toList
            .grouped(batchSize)
            .toList
            .map { batch =>
              // todo I ahave to erase the value :( maybe see why?
//              val array = batch.map(_.value)
              val ast = DomAst(EMsg(
                DieselMsg.STREAMS.STREAM_ONDATASLICE,
                P.of("stream", name) :: P.of("data", batch) :: Nil
              ))
              DieselAppContext ! DEExpand(owner.id, tid, recurse = true, -1, List(ast))
              pickedUp += batch.size
            }

        list.drop(pickedUp)
      } else {
        list.toList.map { data =>
          val ast = DomAst(EMsg(
            DieselMsg.STREAMS.STREAM_ONDATA,
            P.of("stream", name) :: P.of("data", data) :: Nil
          ))
          DieselAppContext ! DEExpand(owner.id, tid, recurse = true, -1, List(ast))
        }

        list.clear()
      }
    }
  }

  def done(justConsume: Boolean = false) = {
    if (!justConsume) isDone = true

    val ast = DomAst(EMsg(
      DieselMsg.STREAMS.STREAM_ONDONE,
      List(
        P.of("stream", name)
      )
    ))

    if (list.size <= 0) {
      targetId.map { tid =>
        DieselAppContext ! DEExpand(owner.id, tid, recurse = true, -1, List(ast))
        DieselAppContext ! DEComplete(owner.id, targetId.get, recurse = true, -1, Nil)
      }
    }
  }

  def error(l: List[P], justConsume: Boolean = false) = {
    if (!justConsume) {
      isError = true
      isDone = true
      errors.appendAll(l)
    }

    val ast = DomAst(EMsg(
      DieselMsg.STREAMS.STREAM_ONERROR,
      List(
        P.of("stream", name)
      ) ::: errors.toList
    ))

    targetId.map { tid =>
      DieselAppContext ! DEExpand(owner.id, targetId.get, recurse = true, -1, List(ast))
      DieselAppContext ! DEComplete(owner.id, targetId.get, recurse = true, -1, Nil)
    }
  }

  // owner starts consuming
  def consume() = {
    // if something already, consume it
    if (list.size > 0) put(Nil, true)
    if (isDone && !isError) done(true)
    if (isDone && isError) error(Nil, true)

    isConsumed = true

    targetId.map { tid =>
      DieselAppContext ! DEComplete(owner.id, targetId.get, recurse = true, -1, Nil)
    }
  }

  // it's cleanup by the owner engine
  def cleanup() = {
    // make sure it's done - don't accept anymore
    isDone = true

    // cleanup
    DieselAppContext ! DESClean(name)
  }
}

//          val ast = DomAst(EMsg(
//            DieselMsg.ENGINE.DIESEL_PONG,
//            List(
//              P.of("parentId", ids(0)),
//              P.of("targetId", ids(1)),
//              P.of("level", -1)
//            )
//          ))

class DomStreamV1(
  owner: DomEngine,
  name: String,
  description: String,
  batch: Boolean = false,
  batchSize: Int = -1,
  correlationId: Option[String] = None) extends DomStream(owner, name, description, batch, batchSize, correlationId) {

}
