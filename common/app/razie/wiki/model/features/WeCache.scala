/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model.features

import razie.wiki.model.{UWID, WikiEntry}

import scala.collection.mutable.ListBuffer

/** caching oft-used pages (categories etc)
  *
  * todo statics are bad, eh? can keep instance per WikiInst
  */
object WeCache {
  var loading=false
  var maxRecs = 500

  private var pre  = new ListBuffer[WikiEntry]()

  // special extra cache of categories per [realm, [name, entry]]
  private var icats = new collection.mutable.HashMap[String,Map[String, WikiEntry]]()

  private var cache = new collection.mutable.HashMap[UWID,WikiEntry]()
  private var depys = new collection.mutable.HashMap[UWID,List[UWID]]()

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
  def put (we:WikiEntry, withDepy:Boolean=true) = synchronized {
    cache.put(we.uwid, we)
    if (we.category == "Category") {
      // categories sit also in the special icats
      val x:Map[String, WikiEntry] = icats.get(we.realm) getOrElse Map.empty
      icats.put(we.realm, x + (we.name -> we))
    }
    if(withDepy)  {
      we.included
      depys.put(we.uwid, we.depys)
    }
  }

  /** update cached wiki and recalculate dependencies */
  def update (we:WikiEntry): Unit = synchronized {
    we.sections
    val more = depys.get(we.uwid).toList.flatten
    cache.put(we.uwid, we)
    depys.put(we.uwid, we.depys)
    more.foreach(u=>update(u.page.get))
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

  def passthrough (ObjectId:String) = synchronized {
    load()
//    icats.getOrElse(realm, Map.empty)
  }

}


