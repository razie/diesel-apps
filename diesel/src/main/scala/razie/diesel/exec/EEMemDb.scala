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
import razie.diesel.engine.DomEngECtx
import razie.diesel.ext.{MatchCollector, _}

import scala.collection.mutable

// same as memdb, but it is shared across all users, like a real micro-service would behave
class EEDieselSharedDb extends EExecutor("diesel.shareddb") {

  /** map of active contexts per transaction */
  val tables = new mutable.HashMap[String, mutable.HashMap[String, P]]()

  override def isMock: Boolean = true

  override def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    m.entity == "diesel.shareddb"
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

      case cmd@("upsert") => {
        if (!tables.contains(col))
          tables.put(col, new mutable.HashMap[String, P]())

        val id = if (ctx("id").length > 0) ctx("id") else new ObjectId().toString

        ctx.getp("document").map(p=>
          tables(col).put(id, p)
        )
        EVal(P("id", id)) :: Nil
      }

      case "findOne" => {
//        val res = tables.get(col).map { col =>
//          col.filter { t =>
//            val o = t._2
//            in.attrs.foldLeft(true)((a, b) => a && o(b.name).toString == b.calculateValue)
//          }
//        }
//        res.headOption.map(x=> EVal(P("document", x._2.toString))).toList
      Nil
      }

      case "log" => {
        val res = tables.keySet.map { k =>
          "Collection: " + k + "\n" +
            tables(k).keySet.map { id =>
              "  " + id + " -> " + tables(k)(id).toString
            }.mkString("  ", "\n", "")
        }.mkString("\n")
        EVal(P("result", res)) :: Nil
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
    EMsg("", "diesel.shareddb", "upsert", Nil) ::
      EMsg("", "diesel.shareddb", "get", Nil) ::
      EMsg("", "diesel.shareddb", "log", Nil) ::
      EMsg("", "diesel.shareddb", "findOne", Nil) ::
      EMsg("", "diesel.shareddb", "remove", Nil) ::
      EMsg("", "diesel.shareddb", "clear", Nil) :: Nil
}

// the context persistence commands - isolated per user and/or anon session
class EEDieselMemDb extends EExecutor("diesel.memdb") {

  /** map of active contexts per transaction */
  case class Col(name: String, entries: mutable.HashMap[String, Any] = new mutable.HashMap[String, Any]())

  case class Session(name: String, var time: Long = System.currentTimeMillis(), tables: mutable.HashMap[String, Col] = new mutable.HashMap[String, Col]())

  val sessions = new mutable.HashMap[String, Session]()

  override def isMock: Boolean = true

  override def test(m: EMsg, cole: Option[MatchCollector] = None)(implicit ctx: ECtx) = {
    m.entity == "diesel.memdb"
  }

  override def apply(in: EMsg, destSpec: Option[EMsg])(implicit ctx: ECtx): List[Any] = synchronized {
    cleanup
    // user id or anon session (one workflow)
    val sessionId = ctx.credentials.getOrElse(ctx.root.asInstanceOf[DomEngECtx].engine.get.id)
    val session = sessions.get(sessionId).getOrElse {
      if (sessions.size > 400)
        throw new IllegalStateException("Too many in-mem db sessions")

      val x = Session(sessionId)
      sessions.put(sessionId, x)
      x
    }
    session.time = System.currentTimeMillis() // should I or not?
    val tables = session.tables

    val col = ctx("collection")

    def log = {
      tables.keySet.map { k =>
        "Collection: " + k + "\n" +
          tables(k).entries.keySet.map { id =>
            "  " + id + " -> " + tables(k).entries(id).toString
          }.mkString("  ", "\n", "")
      }.mkString("\n")
    }

    in.met match {

      case "get" => {
        require(col.length > 0)
        tables.get(col).flatMap(_.entries.get(ctx("id"))).map(x => EVal(P("document", x.toString))).toList
      }

      case cmd@("upsert") => {
        require(col.length > 0)
        if (tables.size > 10)
          throw new IllegalStateException("Too many collections (10)")

        if (!tables.contains(col))
          tables.put(col, Col(col))

        val id = if (ctx("id").length > 0) ctx("id") else new ObjectId().toString

        if (tables(col).entries.size > 15)
          throw new IllegalStateException("Too many entries in collection (15)")

        tables(col).entries.put(id, ctx("document"))
        EVal(P("id", id)) :: Nil
      }

      case "logAll" => {
        val res = s"Sessions: ${sessions.size}\n" + log
        EVal(P("result", res)) :: Nil
      }

      case "log" => {
        EVal(P("result", log)) :: Nil
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
    EMsg("", "diesel.memdb", "upsert", Nil) ::
      EMsg("", "diesel.memdb", "get", Nil) ::
      EMsg("", "diesel.memdb", "log", Nil) ::
      //        EMsg("", "diesel.memdb", "logAll", Nil) :: // undocumented
      EMsg("", "diesel.memdb", "clear", Nil) :: Nil
}


