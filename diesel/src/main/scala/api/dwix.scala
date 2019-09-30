/**
  *   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package api

import razie.tconf.DUser

/**
  * diesel wix - some markers that will need refactoring
  *
  * todo refactor out when moving Website/tenant down here
  */
object dwix {

  /** current or default environment for given realm and user */
  def dieselEnvFor (realm:String, ou:Option[DUser]):String = {
    ou
        .flatMap(_.realmPrefs(realm).get("dieselEnv"))
        .orElse(
          razie.hosting.Website.forRealm(realm).flatMap(_.kind)
        ).getOrElse("local")
  }
}

