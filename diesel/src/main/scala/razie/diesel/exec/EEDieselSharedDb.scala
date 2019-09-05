/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.diesel.exec

import com.mongodb.casbah.Imports._
import razie.diesel.dom.RDOM._
import razie.diesel.dom._
import razie.diesel.ext.{MatchCollector, _}
import scala.collection.mutable

// same as memdb, but it is shared across all users, like a real micro-service would behave
class EEDieselSharedDb extends EExecutor("diesel.db.shareddb") {
  final val SHAREDDB = "diesel.db.shareddb"

  /** map of active contexts per transaction */
  val tables = new mutable.HashMap[String, mutable.HashMap[String, P]]()

  override def isMock: Boolean = true

  override def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    m.entity == SHAREDDB
  }

  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] = synchronized {
    val col = ctx("collection")

    in.met match {

      case "get" => {
        tables.get(col).flatMap(_.get(ctx("id"))).map(x => EVal(x)).toList
      }

      case "remove" => {
        tables.get(col).flatMap(_.remove(ctx("id"))).map(
          x => EVal(x)
        ).toList
      }

      case "upsert" => {
        if (!tables.contains(col))
          tables.put(col, new mutable.HashMap[String, P]())

        val id = if (ctx("id").length > 0) ctx("id") else new ObjectId().toString

        ctx.getp("document").map(p=>
          tables(col).put(id, p)
        )
        EVal(P("id", id)) :: Nil
      }

      case "log" => {
        val res = tables.keySet.map { k =>
          "Collection: " + k + "\n" +
            tables(k).keySet.map { id =>
              "  " + id + " -> " + tables(k)(id).toString
            }.mkString("  ", "\n", "")
        }.mkString("\n")
        EVal(P("payload", res)) :: Nil
      }

      case "clear" => {
        ctx.get("collection").map {col=>
          tables.get(col).map(_.clear())
        }.getOrElse {
          tables.clear()
        }
        Nil
      }

      case s@_ => {
        new EError(s"ctx.$s - unknown activity ") :: Nil
      }
    }
  }

  override def toString = "$executor::shareddb "

  override val messages: List[EMsg] =
    EMsg(SHAREDDB, "upsert") ::
      EMsg(SHAREDDB, "get") ::
      EMsg(SHAREDDB, "log") ::
      EMsg(SHAREDDB, "remove") ::
      EMsg(SHAREDDB, "clear") :: Nil
}


