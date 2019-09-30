/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.wiki.util

import razie.Snakk
import razie.wiki.Sec._
import razie.RString._

object Maps extends razie.Logging {

  def latlong(addr: String): Option[(String, String)] = {
    try {
      val resp = Snakk.json (
        Snakk.url(
          "http://maps.googleapis.com/maps/api/geocode/json?address=" + addr.toUrl + "&sensor=false",
          Map.empty,
          //        Map("privatekey" -> "6Ld9uNASAAAAADEg15VTEoHjbLmpGTkI-3BE3Eax", "remoteip" -> "kk", "challenge" -> challenge, "response" -> response),
          "GET"))

      Some((
        resp \ "results" \ "geometry" \ "location" \@@ "lat",
        resp \ "results" \ "geometry" \ "location" \@@ "lng"))
    } catch {
      case e @ (_ :Throwable) => {
        error ("ERR_COMMS can't geocode address", e)
        None
      }
    }
  }
}


