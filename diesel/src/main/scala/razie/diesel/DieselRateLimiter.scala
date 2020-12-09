/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <    /README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/   /LICENSE.txt
  * */
package razie.diesel

import play.api.mvc.RequestHeader
import razie.wiki.admin.GlobalData
import razie.wiki.model.{WikiConfigChanged, WikiObservers}
import scala.collection.concurrent.TrieMap

/** a rate limit group */
case class RateLimitGroup(name: String, limit: Int, regex: List[String], headers: List[(String, String)])

/** per group rate stats */
case class RateLimitStats(name: String, max: Long, limited: Long)

/** rate limiting stats and config */
object DieselRateLimiter extends razie.Logging {
  // rate limit groups
  val rateLimits = new TrieMap[String, RateLimitGroup]()
  val rateCurrent = new TrieMap[String, Int]()
  val rateStats = new TrieMap[String, RateLimitStats]()

  var RATELIMIT = true // rate limit API requests

  var LIMIT_API = 80

  WikiObservers mini {
    case WikiConfigChanged(node, config) => {
      val threads = config.prop("akka.actor.default-dispatcher.thread-pool-executor.fixed-pool-size", "25").toInt
      LIMIT_API = config.prop("diesel.staticRateLimit", "80").toInt
      RATELIMIT = config.prop("diesel.staticRateLimiting", "true").toBoolean
      log(s"Updated RATE LIMITS to: $LIMIT_API , $RATELIMIT , $threads")
    }
  }

  /** API requests may be rate limited separately */
  def isApiRequest(path: String) = {
    path.contains("/elkpt/")
//    path.startsWith( "/diesel/rest/") ||
//        path.startsWith( "/diesel/mock/") ||
//        path.startsWith( "/diesel/react/") ||
//        path.startsWith( "/diesel/start/") ||
//        path.startsWith( "/diesel/fiddle/react/") ||
//        path.startsWith( "/diesel/wreact/")
  }

  /** main call to serve or limit
    *
    * @param rh          request
    * @param preliminary true if I should not incremeent served stats
    * @param serve       serve request callback
    * @param limited     limited request callback
    * @tparam T
    * @return
    */
  def serveOrLimit[T](rh: RequestHeader, preliminary: Boolean = false)(serve: RequestHeader => T)(limited: (String,
      Long) => T): T = {

    val apiRequest = isApiRequest(rh.path)

    def serveIt() = {
      // only update stats if not preliminary
      if (!preliminary) {
          GlobalData.serving.incrementAndGet()

          if (apiRequest) {
            GlobalData.servingApiRequests.incrementAndGet()
          }
      }

      serve(rh)
    }

    getGroup(rh)
        .map { group =>
          val count = GlobalData.servingApiRequests.get()

          if (RATELIMIT && count > LIMIT_API) {
            GlobalData.limitedApiRequests.incrementAndGet()

            limited.apply(group, count)
          } else {
            serveIt()
          }
        }.getOrElse {
      serveIt()
    }
  }

  def getGroup(rh: RequestHeader): Option[String] = {
//    rateLimits.find(t => t._2.regex.exists(rh.path.matches)).map(_._1)
    if (isApiRequest(rh.path)) Some("api")
    else None
  }

  def start(rh: RequestHeader, group: String) = {
    rateLimits.find(t => t._2.regex.exists(rh.path.matches)).map(_._1)
  }

  def served(rh: RequestHeader, group: String) = {
    rateLimits.find(t => t._2.regex.exists(rh.path.matches)).map(_._1)
  }
}
