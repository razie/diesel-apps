/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <    /README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/   /LICENSE.txt
  * */
package razie.diesel

import java.util.concurrent.atomic.AtomicLong
import play.api.mvc.RequestHeader
import razie.wiki.admin.GlobalData
import razie.wiki.model.{WikiConfigChanged, WikiObservers}
import scala.collection.concurrent.TrieMap

/** a rate limit group */
case class RateLimitGroup(
  name: String,
  limit: Int,
  var regex: List[String],
  var headers: List[(String, String)],
  maxServedCount: AtomicLong = new AtomicLong(0),
  servingCount: AtomicLong = new AtomicLong(0),
  servedCount: AtomicLong = new AtomicLong(0),
  limitedCount: AtomicLong = new AtomicLong(0)
) {
  def decServing() = {
    servingCount.decrementAndGet()
  }
}

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
  var STATIC_PATH = "/elkpt"

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
    // how do we speed this up?
//    path.contains("/elkpt/") ||
    path.startsWith("/diesel/rest/") ||
        path.startsWith("/diesel/mock/")
//        path.startsWith("/diesel/react/") ||
//        path.startsWith("/diesel/start/") ||
//        path.startsWith("/diesel/fiddle/react/") ||
//        path.startsWith("/diesel/wreact/")
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
  def serveOrLimit[T](rh: RequestHeader, preliminary: Boolean = false)(serve: (RequestHeader, Option[RateLimitGroup])
      => T)(limited: (String,
      Long) => T): T = {

    val apiRequest = isApiRequest(rh.path)

    def serveIt(rateLimitGroup: Option[RateLimitGroup]) = {
      // only update stats if not preliminary
      if (!preliminary) {
        GlobalData.serving.incrementAndGet()

        if (apiRequest) {
          GlobalData.servingApiRequests.incrementAndGet()
        }
      }

      serve(rh, rateLimitGroup)
    }

    // find group and rate limit
    getGroup(rh).map { group =>
      val count = group.servingCount.incrementAndGet()

      // note that this is called twice per request
      // when preliminary, the servingCount is decremented elsewhere...

      if (RATELIMIT && count > group.limit) {
        GlobalData.limitedApiRequests.incrementAndGet()
        group.servingCount.decrementAndGet()
        group.limitedCount.incrementAndGet()

        limited.apply(group.name, count)
      } else {
        serveIt(Some(group))
      }
    }.getOrElse {
      serveIt(None)
    }
  }

  def getGroup(rh: RequestHeader): Option[RateLimitGroup] = {
    rateLimits.find(t => t._2.regex.exists(rh.path.matches)).map(_._2)
//    if (isApiRequest(rh.path)) Some("api")
//    else None
  }

  def start(rh: RequestHeader, group: String) = {
    rateLimits.find(t => t._2.regex.exists(rh.path.matches)).map(_._1)
  }

  def served(rh: RequestHeader, group: String) = {
    rateLimits.find(t => t._2.regex.exists(rh.path.matches)).map(_._1)
  }

  def toj = {
    Map(
      "rateLimits" ->
          rateLimits.map(t =>
            t._1 -> Map(
              "maxServed" -> t._2.maxServedCount.get(),
              "serving" -> t._2.servingCount.get(),
              "limit" -> t._2.limit,
              "served" -> t._2.servedCount.get(),
              "limited" -> t._2.limitedCount.get()
            )).toList
              .toMap
    )
  }
}
