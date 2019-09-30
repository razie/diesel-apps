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
  final val MAX = 100

  // stupid LRU expiry
  private var cachel = new collection.concurrent.TrieMap[String, Long]()

  // cache by page content - so versioning embedded
  private var cachem = new collection.concurrent.TrieMap[String,(DSpec,Option[RDomain])]()

  def orcached (we:DSpec, d: =>Option[RDomain]) : Option[RDomain] = {
    val res = cachem.get(we.content).flatMap(_._2).orElse {
        cachem.put(we.content, (we, d))
        cachel.put(we.content, System.currentTimeMillis())

        // prune by size
        if (cachel.size > MAX) {
          var min = System.currentTimeMillis()
          var minc = ""
          cachel.foreach(x => if (x._2 < min) {
            min = x._2
            minc = x._1
          })
          if (minc != we.content) {
            cachel.remove(minc)
            cachem.remove(minc)
          }
        }

        cachem = cachem
        cachel = cachel
        d
    }
    cachel.update(we.content, System.currentTimeMillis())
    res
  }
}


