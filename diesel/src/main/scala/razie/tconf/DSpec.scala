/** ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  * */
package razie.tconf

import razie.diesel.engine.nodes
import razie.diesel.engine.nodes
import razie.tconf.parser.JMapFoldingContext
import razie.wiki.parser.{ParserBase, SimpleSpecParser}

/** a specification - can be a text, a wiki or anything else we can parse to extract some piece of configuration
  *
  * Specifications are meant to be parsed and DOM/diesel elements collected. Also, they are addressable (specPath)
  *
  * We do not concern with the way these are found - that's the inventory, but usually they are passed into an
  * engine etc
  */
trait DSpec {
  def specRef: TSpecRef

  // todo this is too specific - need to refactor out
  def findSection(name: String, tags: String = ""): Option[DTemplate]

  /** find template with predicate */
  def findSection(p: DTemplate => Boolean): Option[DTemplate]

  /** other parsing artifacts to be used by knowledgeable modules.
    * Parsers can put stuff in here. */
  def collector: scala.collection.mutable.HashMap[String, Any]

  /** set during parsing and folding - false if page has any user-specific elements
    * any scripts or such will make this false
    * this is very pessimistic right now for safety issues: even a whiff of non-static content will turn this off
    */
  var cacheable: Boolean = true

  /**
    * original text content, not pre-processed - this is generally useless
    */
  def content: String

  /** the content, pre-processed, with includes and macros expanded etc. But no parsing done
    *
    * NOTE this is not a different representation, like html or something... it is just the content, pre-processed
    *
    * todo should be an Either
    */
  def contentPreProcessed: String

  /** the content, pre-processed, with includes expanded etc.
    *
    * NOTE this is not a different representation, like html or something... it is just the content, pre-processed
    *
    * the assumption is that specs can parse themselves and cache the AST elements
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
  def isDraft: Boolean = false

  /** visibility of this spec - see razie.tconf.Visibility */
  def visibility: String = Visibility.PUBLIC

  /** category - you can categorize the specs */
  def cat: String = ""
}

/** a specification of a template - templates are sections inside specs
  * name = name
  * parmStr = list of tags, really
  */
trait DTemplate {
  def name: String

  def stype: String

  def content: String

  def tags: String

  def specRef: TSpecRef

  def pos: EPos

  def parms: Map[String, String]

  /** resolve a parm, case-insensitive and stip quotes */
  def parm(name: String): Option[String] =
    parms
        .find(_._1.compareToIgnoreCase(name) == 0)
        .map(_._2)
        .map(nodes.stripQuotes)
}

/** can retrieve specs, by wpath and ver */
trait DSpecInventory {
  def findSpec(path: TSpecRef): Option[DSpec]

  def querySpecs(realm: String, q: String, scope: String, curTags: String = "", max: Int = 2000): List[DSpec]
}

/** most specifications are made of a text content, which is parsed */
class BaseTextSpec(val name: String, val text: String, val tags: Seq[String] = Seq()) extends DSpec {
  def specRef: TSpecRef = new SpecRef("", name, name)

  def findSection(name: String, tags: String = ""): Option[DTemplate] = None

  def findSection(p: DTemplate => Boolean): Option[DTemplate] = None

  /** other parsing artifacts to be used by knowledgeable modules.
    * Parsers can put stuff in here. */
  override val collector = new scala.collection.mutable.HashMap[String, Any]()

  override def content: String = text

  override def contentPreProcessed: String = text

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
      p.apply(content).fold(new JMapFoldingContext(Some(this), None)).s
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

/** the simplest spec - from a named string property */
case class TextSpec(override val name: String, override val text: String)
    extends BaseTextSpec(name, text) {}


