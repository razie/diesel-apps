/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.admin

import java.util.concurrent.atomic.AtomicLong
import org.joda.time.DateTime
import scala.concurrent.{Future, Promise}

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

  val dieselEnginesTotal = new AtomicLong(0) // how many engines created
  val dieselEnginesActive = new AtomicLong(0) // how many engines active

  /** how many wiki options have been requested - simple stats */
  var wikiOptions = 0L

  val startedDtm = DateTime.now

  var clusterStatus: String = "-"

  var reactorsLoaded = false
  val reactorsLoadedP: Promise[Boolean] = Promise[Boolean]()
  /** wait here if you need reactors */
  val reactorsLoadedF: Future[Boolean] = reactorsLoadedP.future

  def toMap() = {
    Map(
      "Global.serving" -> GlobalData.serving,
      "Global.served" -> GlobalData.served,
      "Global.servingApiRequests" -> GlobalData.servingApiRequests,
      "Global.servedApiRequests" -> GlobalData.servedApiRequests,
      "Global.limitedApiRequests" -> GlobalData.limitedApiRequests,
      "Global.maxServing" -> GlobalData.maxServing,
      "Global.maxServingApiRequests" -> GlobalData.maxServingApiRequests,
      "Global.dieselEnginesTotal" -> GlobalData.dieselEnginesTotal.get(),
      "Global.dieselEnginesActive" -> GlobalData.dieselEnginesActive.get(),
      "Global.wikiOptions" -> GlobalData.wikiOptions,
      "Global.servedPages" -> GlobalData.servedRequests,
      "Global.startedDtm" -> GlobalData.startedDtm,
      "SendEmail.curCount" -> SendEmail.curCount,
      "SendEmail.state" -> SendEmail.state,
      "ClusterStatus" -> GlobalData.clusterStatus
    )
  }
}

