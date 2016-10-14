package controllers

import razie.RString._
import razie.Snakk

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


