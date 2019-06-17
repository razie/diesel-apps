package razie.tconf.hosting

/** basic support for multi-tenant */
trait Reactors {

  /** realm/tenant specific properties */
  def getProperties (realm:String) : Map[String,String] = Map.empty

  /** realm/tenant specific properties */
  def getProperties (realm:String, prefix:String) : Map[String,String] = getProperties(realm).filter(_._1.startsWith(prefix))

  /** realm/tenant specific properties */
  def forEachProperty (realm:String, prefix:String) (f:PartialFunction[(String,String), Unit]) : Unit = getProperties(realm).filter(_._1.startsWith(prefix)).foreach(f)
}

object Reactors {
  /** initialized/injected in Module/Global */
  var impl = new Reactors {}
}

