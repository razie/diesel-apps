package razie.diesel.dom

/** uniquely identifies a piece of specification
  *
  * @param source - the source system: inventory understands and delegates to
  * @param wpath - unique id of the spec
  * @param ver - optionally identify a version of the spec
  * @param draft - optionally identify a certain temporary variant (i.e. autosaved by username)
  */
case class SpecPath (source:String, wpath:String, ver:Option[String]=None, draft:Option[String]=None)

/** a specification - can be a text, a wiki or anything else we can parse to extract a diesel */
trait DSpec {
  def specPath : SpecPath
  def findTemplate (name:String) : Option[DTemplate]
}

/** a specification of a template */
trait DTemplate {
  def content : String
  def parmStr : String
  def specPath : SpecPath
  def pos : EPos

  def parms =
    if(parmStr.trim.length > 0)
      parmStr.split(",").map(s=>s.split("=")).map(a=> (a(0), a(1))).toMap
    else Map.empty[String,String]

}

/** can retrieve specs, by wpath and ver */
trait DSpecInventory {
  def find (path:SpecPath) : Option[DSpec]
}


