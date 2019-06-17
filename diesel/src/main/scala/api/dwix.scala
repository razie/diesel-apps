package api

import razie.tconf.DUser

/** todo refactor out when moving Website/tenant down here */
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

