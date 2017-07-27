package razie.wiki.util

import play.api.mvc.{RequestHeader, AnyContent, Request}
import razie.wiki.Services

/**
 * just some play request utils
 */
object PlayTools {
  /** get the host that was forwarded here - used for multi-site hosting */
  def getHost (implicit request: RequestHeader) =
    if(Services.config.isLocalhost)
      Some(Services.config.simulateHost)
    else
      request.headers.get("X-FORWARDED-HOST").map(_.replaceFirst(":.*$", "")) // remove port number

  /** assume the request was a post - get the data reformatted as simpel NVP */
  def postData (implicit request : Request[AnyContent]) = {
    val x = request.body.asFormUrlEncoded
    request.body.asFormUrlEncoded.map(_.collect { case (k, v) => (k, v.head) }).getOrElse(Map.empty)
  }
}
