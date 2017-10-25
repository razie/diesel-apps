package razie.tconf

import razie.diesel.dom._
import razie.diesel.ext

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
  def ahref:Option[String]
}

case class SpecPath (
  source:String,
  wpath:String,
  realm:String,
  ver:Option[String]=None,
  draft:Option[String]=None) extends TSpecPath {

  def ahref:Option[String] = None
}

/** a specification - can be a text, a wiki or anything else we can parse to extract a diesel
  *
  * Specifications are meant to be parsed and DOM/diesel elements collected.
  *
  * We do not concern with the way these are found but with
  */
trait DSpec {
  def specPath : TSpecPath

  // todo this is too specific - need to refactor out
  def findTemplate (name:String, direction:String="") : Option[DTemplate]

  /** other parsing artifacts to be used by knowledgeable modules.
    * Parsers can put stuff in here. */
  def collector : scala.collection.mutable.HashMap[String, Any]

  /** set during parsing and folding - false if page has any user-specific elements
    *  any scripts or such will make this false
    *  this is very pessimistic right now for safety issues: even a whiff of non-static content will turn this off
    */
  var cacheable: Boolean = true

  /** the assumption is that specs can parse themselves and cache the AST elements
    *
    * errors must contain "CANNOT PARSE" and more information
    *
    * todo parsed should be an Either
    */
  def parsed : String
}

/** a specification of a template */
trait DTemplate {
  def content : String
  def parmStr : String
  def specPath : TSpecPath
  def pos : EPos
  def parms : Map[String,String]

  /** resolve a parm, case-insensitive and stip quotes */
  def parm(name: String): Option[String] =
    parms
      .find(_._1.compareToIgnoreCase(name) == 0)
      .map(_._2)
      .map(ext.stripQuotes)

  /** template attributes, like content-type etc */
//  lazy val parms =
//    if(parmStr.trim.length > 0)
//      parmStr.trim.split("[, \n\t]").map(s=>s.split("=")).filter(_.size == 2).map(a=> (a(0).trim, a(1).trim)).toMap
//    else Map.empty[String,String]

}

/** can retrieve specs, by wpath and ver */
trait DSpecInventory {
  def find (path:TSpecPath) : Option[DSpec]
}

/** the simplest spec - from a named string property */
case class TextSpec (val name:String, val text:String) extends DSpec {
  def specPath : TSpecPath = new SpecPath("local", name, "")

  def findTemplate (name:String, direction:String="") : Option[DTemplate] = None

  /** other parsing artifacts to be used by knowledgeable modules.
    * Parsers can put stuff in here. */
  val collector = new scala.collection.mutable.HashMap[String, Any]()

  /** the assumption is that specs can parse themselves and cache the AST elements
    *
    * errors must contain "CANNOT PARSE" and more information
    *
    * todo parsed should be an Either
    */
  private var iparsed : Option[String] = None

  // parse just once
  def parsed : String = iparsed.getOrElse {
    val res = {
      "x"
    }
    iparsed = Some(res)
    res
  }
}


