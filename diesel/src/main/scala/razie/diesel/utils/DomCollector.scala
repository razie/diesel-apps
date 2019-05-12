/**
  *   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.utils

import org.joda.time.DateTime
import razie.diesel.engine.DomEngine
import razie.diesel.ext.EMsg

// todo make instance in Reactor instead of a static
/** collects the last engine traces */
object DomCollector {

  final val MAX_SIZE = 200 // how many to keep
  final val MAX_SIZE_LOWER = 30 // how many to keep

  case class CollectedAst(stream:String, realm:String, id:String, userId:Option[String], engine:DomEngine, details:String, dtm:DateTime=DateTime.now) {
    def isLowerPriority = {
      val first = engine.root.children.headOption.map(_.value).collect { case e:EMsg => e}
      first.exists{m=>
        val ea = m.ea
        ea == "diesel.guardian.poll" ||
        ea == "diesel.realm.loaded"
      }
    }
  }

  // statically collecting the last 100 results sets
  private var asts: List[CollectedAst] = Nil

  def withAsts[T] (f: List[CollectedAst] =>T) : T = asts.synchronized {
    f(asts)
  }

  /** statically collect more asts */
  def collectAst (stream:String, realm:String, xid:String, userId:Option[String], eng:DomEngine, details:String="") = synchronized {
    val newAsts = CollectedAst(stream, realm, xid, userId, eng, details) :: asts.filter(_.id != xid)
    val lower = asts.filter(_.isLowerPriority)

    if(lower.size > MAX_SIZE_LOWER) {
      val lastId = lower.last.id
      // try to remove first a lower priority one
      asts = newAsts.filter(_.id != lastId).take(MAX_SIZE - 1)
    } else {
      asts = newAsts.take(MAX_SIZE - 1)
    }
  }

  /** statically collect more asts */
  def cleanAst = synchronized {
    asts = Nil
  }

}

