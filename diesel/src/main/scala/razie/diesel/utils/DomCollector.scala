/**
  *   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.utils

import org.joda.time.DateTime
import razie.diesel.dom.DomAst

// todo make instance in Reactor instead of a static
/** this is the default engine per reactor and user, continuously running all the stories */
object DomCollector {

  case class CollectedAst(stream:String, id:String, root:DomAst, details:String, dtm:DateTime=DateTime.now)

  // statically collecting the last 100 results sets
  private var asts: List[CollectedAst] = Nil

  def withAsts[T] (f: List[CollectedAst] =>T) = asts.synchronized {
    f(asts)
  }

  /** statically collect more asts */
  def collectAst (stream:String, xid:String, root:DomAst, details:String="") = synchronized {
    if (asts.size > 100) asts = asts.take(99)
    asts = CollectedAst(stream, xid, root, details) :: asts
  }

}

