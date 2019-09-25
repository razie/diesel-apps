package razie.tconf

import razie.diesel.ext
import razie.tconf.parser.JMapFoldingContext
import razie.wiki.parser.{ParserBase, SimpleSpecParser}

/** a specification - can be a text, a wiki or anything else we can parse to extract some piece of configuration
  *
  * Specifications are meant to be parsed and DOM/diesel elements collected.
  *
  * We do not concern with the way these are found
  */
trait DSpec {
  def specPath: TSpecPath

  // todo this is too specific - need to refactor out
  def findSection(name: String, tags: String = ""): Option[DTemplate]

  /** find template with predicate */
  def findSection(p: DTemplate => Boolean): Option[DTemplate]

  /** other parsing artifacts to be used by knowledgeable modules.
    * Parsers can put stuff in here. */
  def collector: scala.collection.mutable.HashMap[String, Any]

  /** set during parsing and folding - false if page has any user-specific elements
    *  any scripts or such will make this false
    *  this is very pessimistic right now for safety issues: even a whiff of non-static content will turn this off
    */
  var cacheable: Boolean = true

  /**
    * original text content
    */
  def content: String

  /** the assumption is that specs can parse themselves and cache the AST elements
    *
    * errors must contain "CANNOT PARSE" and more information
    *
    * todo parsed should be an Either
    */
  def parsed: String

  /** all properties contained in this spec, in various forms */
  def allProps: Map[String, String]

  def tags: Seq[String]

  /** we support draft specs vs non=draft (published) */
  def isDraft : Boolean = false

  /** visibility of this spec - see razie.tconf.Visibility */
  def visibility : String = Visibility.PUBLIC

  /** category - you can categorize the specs */
  def cat : String = ""
}

/** a specification of a template
  * name = name
  * parmStr = list of tags, really
  */
trait DTemplate {
  def name: String
  def stype: String
  def content: String
  def tags: String
  def specPath: TSpecPath
  def pos: EPos
  def parms: Map[String, String]

  /** resolve a parm, case-insensitive and stip quotes */
  def parm(name: String): Option[String] =
    parms
      .find(_._1.compareToIgnoreCase(name) == 0)
      .map(_._2)
      .map(ext.stripQuotes)
}

/** can retrieve specs, by wpath and ver */
trait DSpecInventory {
  def findSpec(path: TSpecPath): Option[DSpec]
}

/** the simplest spec - from a named string property */
case class TextSpec(override val name: String, override val text: String)
    extends BaseTextSpec(name, text) {}

/** most specifications are made of a text content, which is parsed */
class BaseTextSpec(val name: String, val text: String, val tags:Seq[String] = Seq()) extends DSpec {
  def specPath: TSpecPath = new SpecPath("local", name, "")

  def findSection(name: String, tags: String = ""): Option[DTemplate] = None
  def findSection(p: DTemplate => Boolean): Option[DTemplate] = None

  /** other parsing artifacts to be used by knowledgeable modules.
    * Parsers can put stuff in here. */
  override val collector = new scala.collection.mutable.HashMap[String, Any]()

  override def content: String = text

  /** the assumption is that specs can parse themselves and cache the AST elements
    *
    * errors must contain "CANNOT PARSE" and more information
    *
    * todo parsed should be an Either
    */
  private var iparsed: Option[BaseTextSpec] = None
  private var sparsed: Option[String] = None

  // parse just once
  def parsed: String = sparsed.getOrElse {
    val res = {
      val p = mkParser
      p.apply(text).fold(new JMapFoldingContext(Some(this), None)).s
    }
    sparsed = Some(res)
    iparsed = Some(this)
    res
  }

  def mkParser: ParserBase = new SimpleSpecParser {

    /** provide a realm */
    override def realm: String = "rk"
  }

  def xparsed: BaseTextSpec = iparsed.getOrElse {
    parsed
    iparsed.get
  }

  /** all properties contained in this spec, in various forms */
  def allProps: Map[String, String] = Map.empty
}
