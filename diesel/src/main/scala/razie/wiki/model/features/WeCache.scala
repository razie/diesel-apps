/** ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  * */
package razie.wiki.model.features

import razie.wiki.model.{UWID, WikiEntry}
import scala.collection.mutable.ListBuffer

/** caching oft-used pages (categories etc)
  *
  * todo statics are bad, eh? to keep instance per WikiInst
  */
object WeCache {
  var loading=false
  var maxRecs = 500

  private var pre  = new ListBuffer[WikiEntry]()

  // todo mt-safe use collection.concurrent.TrieMap ?

  // special extra cache of categories per [realm, [name, entry]]
  private var icats = new collection.mutable.HashMap[String,Map[String, WikiEntry]]()

  // actual cache
  private var cache = new collection.mutable.HashMap[String,WikiEntry]()

  // dependencies between pages, includes etc me -> who depends on me
  private var depys = new collection.mutable.HashMap[String,List[UWID]]()

  /** while starting up, we can't process wikis */
  def preload (we:WikiEntry) = {
    pre += we
    put(we, false)
  }

  /** lazy, at the end of starting up - process preloaded wikis */
  def load() = {
    if(!loading && pre.nonEmpty) {
      loading=true
      pre foreach update
      pre.clear()
    }
  }

  /** cache wiki */
  def put (we:WikiEntry, withDepy:Boolean=true, preprocess:Boolean = true) = synchronized {
    cache.put(we._id.toString, we)

    if (we.category == "Category" ) {
      // categories sit also in the special icats
      val x:Map[String, WikiEntry] = icats.get(we.realm) getOrElse Map.empty
      icats.put(we.realm, x + (we.name -> we))
    }

    if(withDepy)  {
      if(preprocess) we.included

      // todo cleanup by removing old depys
      we.depys.map {d =>
        // filter myself since I'll add me again anyways
        val cur = depys.getOrElse(d.id.toString, Nil).filter(_.id != we._id)
        depys.put (d.id.toString, we.uwid :: cur)
      }
    }
  }

  /** update cached wiki and recalculate dependencies */
  def update (we:WikiEntry): Unit = synchronized {
    we.sections // calculate includes etc

    // find dependents
    val dependants = depys.get(we._id.toString).toList.flatten

    put(we, true, false)

    dependants.foreach(u=>update(u.page.get))
  }

  def get (realm:String, cat:String, name:String) = synchronized {
    load()
    cache.find(t=> t._2.realm == realm && t._2.category == cat && t._2.name == name)
  }

  def list (realm:String, cat:String) = synchronized {
    load()
    cache.filter(t=> t._2.realm == realm && t._2.category == cat).values.toList
  }

  def cats (realm:String) = synchronized {
    load()
    icats.getOrElse(realm, Map.empty)
  }
}
