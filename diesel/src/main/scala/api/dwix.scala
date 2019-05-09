package api

import razie.wiki.model._

/** this is available to scripts inside the wikis */
object dwix {

  def dieselEnvFor (realm:String, ou:Option[WikiUser]):String = {
    ou
      .flatMap(_.realmPrefs(realm).get("dieselEnv"))
      .orElse(
        razie.hosting.Website.forRealm(realm).flatMap(_.kind)
      ).getOrElse("local")
  }
}

