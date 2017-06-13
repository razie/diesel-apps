package razie.diesel.dom

/** uniquely identifies a piece of specification
  *
  * for identifying sections inside a larger document, the wpath will include like a section pointer (...#section)
  *
  * @param source - the source system: inventory understands and delegates to
  * @param wpath - unique id of the spec
  * @param realm - optionally identify a realm within the source
  * @param ver - optionally identify a version of the spec
  * @param draft - optionally identify a certain temporary variant (i.e. autosaved by username)
  */
trait TSpecPath {
  def source:String
  def wpath:String
  def realm:String
  def ver:Option[String]
  def draft:Option[String]
}

case class SpecPath (source:String, wpath:String, realm:String, ver:Option[String]=None, draft:Option[String]=None) extends TSpecPath

/** a specification - can be a text, a wiki or anything else we can parse to extract a diesel
  *
  * Specifications are meant to be parsed and DOM/diesel elements collected.
  *
  * We do not concern with the way these are found but with
  */
trait DSpec {
  def specPath : TSpecPath
  def findTemplate (name:String) : Option[DTemplate]

  /** other parsing artifacts to be used by knowledgeable modules.
    * Parsers can put stuff in here. */
  def cache : scala.collection.mutable.HashMap[String, Any]

  /** set during parsing and folding - false if page has any user-specific elements
    *  any scripts or such will make this false
    *  this is very pessimistic right now for safety issues: even a whiff of non-static content will turn this off
    */
  var cacheable: Boolean = true
}

/** a specification of a template */
trait DTemplate {
  def content : String
  def parmStr : String
  def specPath : TSpecPath
  def pos : EPos

  def parms =
    if(parmStr.trim.length > 0)
      parmStr.split(",").map(s=>s.split("=")).map(a=> (a(0), a(1))).toMap
    else Map.empty[String,String]

}

/** can retrieve specs, by wpath and ver */
trait DSpecInventory {
  def find (path:TSpecPath) : Option[DSpec]
}


