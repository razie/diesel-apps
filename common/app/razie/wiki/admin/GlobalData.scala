/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.admin

import org.joda.time.DateTime

/** current ops data is updated here
  *
  * not nice, poor man's JMX
  */
object GlobalData {
  //todo use proper JMX istead

  // how many requests were served
  var served = 0L
  var servedPages = 0L

  /** how many wiki options have been requested - simple stats */
  var wikiOptions = 0L

  // how many threads are currently serving - if 0, there's none...
  var serving = 0

  val startedDtm = DateTime.now

  var clusterStatus : String = "-"
}

