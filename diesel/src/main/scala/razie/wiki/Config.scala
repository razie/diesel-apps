/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.wiki

import razie.wiki.model.{WikiEntry, WikiEvent, WikiObservers}

/**
  * configuration static
  *
  * todo config should be injected not static
  */
object Config extends WikiConfig {
  final val curYear = "2017" // just FYI basicaly, each club has its own year

  override def simulateHost = isimulateHost
  var isimulateHost = {
    "www.dieselapps.com"    // for testing locally
  }

  val REFERENCE_SIMULATE_HOST = {
    "www.dieselapps.com"    // Do not change this
  }

  WikiObservers mini {
    case WikiEvent(_, "WikiEntry", _, Some(x), _, _, _)
      if "Admin" == x.asInstanceOf[WikiEntry].category && WikiConfig.CFG_PAGES.contains(x.asInstanceOf[WikiEntry].name)  => {
        reloadUrlMap
    }
  }

}
