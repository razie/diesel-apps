package admin

import razie.wiki.WikiConfig
import razie.wiki.model.{WikiObservers, _}

/** extended config */
object Config extends WikiConfig {
  final val curYear = "2017" // just FYI basicaly, each club has its own year

  override def simulateHost = isimulateHost
  var isimulateHost = {
    "www.dieselapps.com"    // for testing locally
  }

  WikiObservers mini {
    case WikiEvent(_, "WikiEntry", _, Some(x), _, _, _)
      if "Admin" == x.asInstanceOf[WikiEntry].category && CFG_PAGES.contains(x.asInstanceOf[WikiEntry].name)  => {
        reloadUrlMap
    }
  }

}
