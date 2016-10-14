/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package model

import play.api.mvc.Request
import razie.wiki.WikiConfig
import razie.wiki.model._
import razie.wiki.util.PlayTools

/** deal with multihosting - identify the reactor from the current domain
  *
  * todo inject and remove hardcodings
  */
object RkReactors {
  /** for the host REACTOR.dslapps.com - see if there is a known reactor for that */
  def forHost (h:String) : Option[String] = {
    // auto-websites of type REACTOR.coolscala.com
    if (h.endsWith(".coolscala.com") ||
      h.endsWith(".wikireactor.com") ||
      h.endsWith(".dslcloud.com") ||    /* the testing website */
      h.endsWith(".dslapps.com") ||
      h.endsWith(".dieselapps.com")
    ) {
      // extract reactor
      val r = h.substring(0, h.indexOf('.')).toLowerCase
      WikiReactors.lowerCase.get(WikiReactors.ALIASES.getOrElse(r, r))
    } else {
      // try to find one that has a website section with this url
      // todo keep parsing the webiste section all the time
      WikiReactors.reactors.values.find(_.we.flatMap(_.section("section", "website")).exists {s=>
        val propSeq = WikiConfig.parsep(s.content)

        propSeq.exists(t=>t._1 == "domain" && t._2 == h)
      }).map(_.realm)
    }
  }

  def apply (request:Request[_]) = PlayTools.getHost(request).flatMap(forHost)
}
