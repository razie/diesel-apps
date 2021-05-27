/**
  *   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.utils

import razie.diesel.dom.RDomain
import razie.tconf.DSpec

/**
  * cache of domain and parsed specs and expiry
  *
  * it is important to cache specs, both draft and final, to ensure low latency
  * to users, for content assist and such
  */
object SpecCache {

  // todo configure max entries - monitor with mem per box size etc
  final val MAX = 140

  // stupid LRU expiry
  private var cachel = new collection.concurrent.TrieMap[String, Long]()

  // cache by page content - so versioning embedded
  private var cachem = new collection.concurrent.TrieMap[String,(DSpec,Option[RDomain])]()

  def orcached (we:DSpec, d: =>Option[RDomain]) : Option[RDomain] = {
    // include realm in index so we don't get domains from other realms, very confusing - Domain entities keep refs
    // to original specs - also wpath so if two are identical, there's no confusion...
    val key = we.specRef.realm + we.specRef.wpath + we.content
    val res = cachem.get(key).flatMap(_._2).orElse {
      cachem.put(key, (we, d))
      cachel.put(key, System.currentTimeMillis())

      // prune by size
      if (cachel.size > MAX) {
        var min = System.currentTimeMillis()
        var minc = ""
        cachel.foreach(x => if (x._2 < min) {
          min = x._2
          minc = x._1
        })
        if (minc != key) {
          cachel.remove(minc)
          cachem.remove(minc)
        }
      }

      cachem = cachem
      cachel = cachel
      d
    }
    cachel.update(key, System.currentTimeMillis())
    res
  }
}


