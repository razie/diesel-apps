/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.admin

import java.util.concurrent.atomic.AtomicLong
import org.joda.time.DateTime
import razie.diesel.DieselRateLimiter
import razie.diesel.engine.DieselAppContext
import razie.diesel.utils.DomCollector
import razie.wiki.Config
import scala.concurrent.{Future, Promise}

/** current ops data is updated here from all over - you can inspect this in a page
  *
  * not nice, poor man's JMX
  */
object GlobalData {
  //todo use proper JMX istead

  // how many requests were served
  val served = new AtomicLong(0)
  val servedRequests = new AtomicLong(0)
  val maxServing = new AtomicLong(0) // how many threads are currently serving - if 0, there's none...
  val maxServingApiRequests = new AtomicLong(0) // how many threads are currently serving - if 0, there's none...
  val serving = new AtomicLong(0) // how many threads are currently serving - if 0, there's none...
  val servingApiRequests = new AtomicLong(0) // how many threads are currently serving - if 0, there's none...
  val servedApiRequests = new AtomicLong(0)
  val limitedApiRequests = new AtomicLong(0) // how many were kicked off under load

  val dieselEnginesTotal = new AtomicLong(0) // how many engines created since start
  val dieselEnginesActive = new AtomicLong(0) // how many engines active now
  val dieselStreamsTotal = new AtomicLong(0) // how many streams created since start
  val dieselStreamsActive = new AtomicLong(0) // how many streams active now

  val dieselCronsActive = new AtomicLong(0) // how many crons active
  val dieselCronsTotal = new AtomicLong(0)   // how many crons triggered since start

  /** how many wiki options have been requested - simple stats */
  var wikiOptions = 0L

  val startedDtm = DateTime.now

  var clusterStatus: String = "-"

  var reactorsLoaded = false
  val reactorsLoadedP: Promise[Boolean] = Promise[Boolean]()

  /** wait here if you need reactors */
  def reactorsLoadedF: Future[Boolean] = reactorsLoadedP.future

  def toMap() = {
    Map(
      "Global.maxConfThreads" -> Config.prop("akka.actor.default-dispatcher.thread-pool-executor.fixed-pool-size"),

      "Global.serving" -> GlobalData.serving.get(),
      "Global.served" -> GlobalData.served.get(),
      "Global.servingApiRequests" -> GlobalData.servingApiRequests.get(),
      "Global.servedApiRequests" -> GlobalData.servedApiRequests.get(),
      "Global.limitedApiRequests" -> GlobalData.limitedApiRequests.get(),
      "Global.maxServing" -> GlobalData.maxServing.get(),
      "Global.maxServingApiRequests" -> GlobalData.maxServingApiRequests.get(),
      "Global.dieselEnginesTotal" -> GlobalData.dieselEnginesTotal.get(),
      "Global.dieselEnginesActive" -> GlobalData.dieselEnginesActive.get(),
      "Global.wikiOptions" -> GlobalData.wikiOptions,
      "Global.servedPages" -> GlobalData.servedRequests.get(),
      "Global.startedDtm" -> GlobalData.startedDtm,
      "SendEmail.curCount" -> SendEmail.curCount,
      "SendEmail.state" -> SendEmail.state,
      "ClusterStatus" -> GlobalData.clusterStatus,
      "Diesel.collectedAst" -> DomCollector.withAsts(_.size),
      "Diesel.activeEngines" -> DieselAppContext.activeEngines.size,
      "Diesel.activeActors" -> DieselAppContext.activeActors.size,
      "Diesel.activeStreams" -> DieselAppContext.activeStreams.size
    ) ++ DieselRateLimiter.toj
  }

  def perfMap() = {
    Map(
      "maxConfThreads" -> Config.prop("akka.actor.default-dispatcher.thread-pool-executor.fixed-pool-size"),
      "serving" -> GlobalData.serving.get(),
      "servingApiRequests" -> GlobalData.servingApiRequests.get(),
      "limitedApiRequests" -> GlobalData.limitedApiRequests.get(),
      "maxServing" -> GlobalData.maxServing.get(),
      "maxServingApiRequests" -> GlobalData.maxServingApiRequests.get(),
      "dieselEnginesTotal" -> GlobalData.dieselEnginesTotal.get(),
      "dieselEnginesActive" -> GlobalData.dieselEnginesActive.get(),
      "activeEngines" -> DieselAppContext.activeEngines.size,
      "activeActors" -> DieselAppContext.activeActors.size,
      "activeStreams" -> DieselAppContext.activeStreams.size
    )
  }
}

