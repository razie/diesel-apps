/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package mod.diesel.model.exec

import com.mongodb.casbah.Imports._
import razie.diesel.dom.RDOM._
import razie.diesel.dom._
import razie.diesel.exec.EExecutor
import razie.diesel.ext.{MatchCollector, _}
import scala.collection.mutable

// the context persistence commands - isolated per user and/or anon session
class EEDieselMemDb extends EExecutor("diesel.db.memdb") {
  final val DB = "diesel.db.memdb"

  /** map of active contexts per transaction */
  case class Col(name: String, entries: mutable.HashMap[String, Any] = new mutable.HashMap[String, Any]())

  case class Session(name: String, var time: Long = System.currentTimeMillis(), tables: mutable.HashMap[String, Col] = new mutable.HashMap[String, Col]())

  val sessions = new mutable.HashMap[String, Session]()

  override def isMock: Boolean = true

  override def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    m.entity == DB
  }

  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] = synchronized {
    cleanup
    // user id or anon session (one workflow)
    val sessionId = ctx.credentials.getOrElse(ctx.root.engine.get.id)
    val session = sessions.get(sessionId).getOrElse {
      if (sessions.size > 400)
        throw new IllegalStateException("Too many in-mem db sessions")

      val x = Session(sessionId)
      sessions.put(sessionId, x)
      x
    }
    session.time = System.currentTimeMillis() // should I or not?
    val tables = session.tables


    def log = {
      tables.keySet.map { k =>
        "Collection: " + k + "\n" +
          tables(k).entries.keySet.map { id =>
            "  " + id + " -> " + tables(k).entries(id).toString
          }.mkString("  ", "\n", "")
      }.mkString("\n")
    }

    in.met match {

      case "get" | "getsert" => {
        val col = ctx.getRequired("collection")
        require(col.length > 0)
        tables.get(col).flatMap(_.entries.get(ctx.getRequired("id"))).map(x => EVal(P("document", x.toString))).toList
      }

      case cmd@("upsert") => {
        val col = ctx.getRequired("collection")
        require(col.length > 0)
        if (tables.size > 10)
          throw new IllegalStateException("Too many collections (10)")

        if (!tables.contains(col))
          tables.put(col, Col(col))

        val id = if (ctx.getRequired("id").length > 0) ctx.getRequired("id") else new ObjectId().toString

        if (tables(col).entries.size > 15)
          throw new IllegalStateException("Too many entries in collection (15)")

        tables(col).entries.put(id, ctx("document"))
        EVal(P("id", id)) :: Nil
      }

      case "logAll" => {
        val res = s"Sessions: ${sessions.size}\n" + log
        EVal(P("payload", res)) :: Nil
      }

      case "log" => {
        EVal(P("payload", log)) :: Nil
      }

      case "clear" => {
        tables.clear()
        Nil
      }
      case s@_ => {
        new EError(s"ctx.$s - unknown activity ") :: Nil
      }
    }
  }

  def cleanup = {
    val oldies = sessions.filter(System.currentTimeMillis() - _._2.time > 10 * 60 * 1000)
    for (elem <- oldies) sessions.remove(elem._1)
  }

  override def toString = "$executor::memdb "

  override val messages: List[EMsg] =
    EMsg(DB, "upsert") ::
      EMsg(DB, "get") ::
      EMsg(DB, "log") ::
      EMsg(DB, "clear") :: Nil
}

