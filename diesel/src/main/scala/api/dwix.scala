/**   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package api

import razie.diesel.engine.DieselAppContext
import razie.hosting.Website
import razie.tconf.DUser

/**
  * diesel wix - some markers that will need refactoring
  *
  * todo refactor out when moving Website/tenant down here
  */
object dwix {

  /** current or default environment for given realm and user
    *
    * 1. user prefs (as set from navbar)
    * 2. realm props - set in EnvironmentSettings with `diesel.realm.set`
    * 3. if not simple mode, infer from website.prop
    * 4. finally default to reactor type dev/prod/qa etc
    * 5. if all else fails, then "local"
    */
  def dieselEnvFor (realm:String, ou:Option[DUser]):String = {
    ou
        .flatMap( _.realmPrefs(realm).get("dieselEnv"))
        .orElse(  Website.getRealmProp(realm, "diesel.env"))
        .orElse(
          if(DieselAppContext.simpleMode) None
          else {
            val w = Website.forRealm(realm)
                w
                .flatMap(_.prop("diesel.env"))
                .filter(_.length > 0)
                .orElse {
                      w.flatMap(_.kind) // dev vs prod etc
                }
          }
        ).getOrElse("local")
  }
}

