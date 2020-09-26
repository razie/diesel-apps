/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.admin

import org.joda.time.DateTime

/** current ops data is updated here from all over - you can inspect this in a page
  *
  * not nice, poor man's JMX
  */
object GlobalData {
  //todo use proper JMX istead

  // how many requests were served
  @volatile var served = 0L
  @volatile var servedRequests = 0L
  @volatile var maxServing = 0 // how many threads are currently serving - if 0, there's none...
  @volatile var maxServingApiRequests = 0 // how many threads are currently serving - if 0, there's none...
  @volatile var serving = 0 // how many threads are currently serving - if 0, there's none...
  @volatile var servingApiRequests = 0 // how many threads are currently serving - if 0, there's none...
  @volatile var servedApiRequests = 0L
  @volatile var limitedApiRequests = 0L // how many were kicked off under load

  /** how many wiki options have been requested - simple stats */
  var wikiOptions = 0L

  val startedDtm = DateTime.now

  var clusterStatus : String = "-"
}

