/**
  *   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.utils

import org.joda.time.DateTime
import razie.diesel.engine.DomEngine

// todo make instance in Reactor instead of a static
/** this is the default engine per reactor and user, continuously running all the stories */
object DomCollector {

  final val MAX_SIZE = 100 // how many to keep

  case class CollectedAst(stream:String, id:String, engine:DomEngine, details:String, dtm:DateTime=DateTime.now)

  // statically collecting the last 100 results sets
  private var asts: List[CollectedAst] = Nil

  def withAsts[T] (f: List[CollectedAst] =>T) : T = asts.synchronized {
    f(asts)
  }

  /** statically collect more asts */
  def collectAst (stream:String, xid:String, eng:DomEngine, details:String="") = synchronized {
    if (asts.size > MAX_SIZE) asts = asts.take(MAX_SIZE-1)
    asts = CollectedAst(stream, xid, eng, details) :: asts.filter(_.id != xid)
  }

  /** statically collect more asts */
  def cleanAst = synchronized {
    asts = Nil
  }

}

