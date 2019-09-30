/**
  *   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.tconf.hosting

/** basic support for multi-tenant
  *
  * A reactor is a separate project/website - the aim is to have logical separation,
  * while serving them all from one server
  */
trait Reactors {

  /** realm/tenant specific properties */
  def getProperties (realm:String) : Map[String,String] = Map.empty

  /** realm/tenant specific properties */
  def getProperties (realm:String, prefix:String) : Map[String,String] = {
    getProperties(realm).filter(_._1.startsWith(prefix))
  }

  /** realm/tenant specific properties */
  def forEachProperty (realm:String, prefix:String) (f:PartialFunction[(String,String), Unit]) : Unit = {
    getProperties(realm).filter(_._1.startsWith(prefix)).foreach(f)
  }
}

object Reactors {
  /** initialized/injected in Module/Global - see [WikiReactors] */
  var impl = new Reactors {}
}

