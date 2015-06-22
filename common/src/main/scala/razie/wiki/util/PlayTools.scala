package razie.wiki.util

import play.api.mvc.{RequestHeader, AnyContent, Request}
import razie.wiki.Services

/**
 * Created by raz on 2014-11-18.
 */
object PlayTools {
  /** get the host that was forwarded here - used for multi-site hosting */
  def getHost (implicit request: RequestHeader) =
    if(Services.config.isLocalhost)
      Some(Services.config.simulateHost)
    else
      request.headers.get("X-FORWARDED-HOST")

  /** assume the request was a post - get the data reformatted as simpel NVP */
  def postData (implicit request : Request[AnyContent]) =
    // somehow i get list of values?
    request.body.asFormUrlEncoded.map(_.collect { case (k, v :: r) => (k, v) }).getOrElse(Map.empty)
}
