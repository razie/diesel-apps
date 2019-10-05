/**
  *   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.wiki.parser

import razie.Snakk
import razie.tconf.BaseTextSpec

/** the simplest spec used in samples and tests - uses the Diesel-Dom parser */
case class DieselTextSpec (override val name:String, override val text:String) extends BaseTextSpec(name, text) {
  override def mkParser = new DieselTextParser("rk")

  def + (other:DieselTextSpec) : DieselTextSpec = {
    DieselTextSpec(name+other.name, text+other.text)
  }
}

/** A simple parser for diesel specs
  *
  * DomParser is the actual Diesel/Dom parser.
  * We extend from it to include its functionality and then we add its parsing rules with withBlocks()
  */
class DieselTextParser(val realm: String) extends SimpleSpecParser with DomParser {
  // include the rules to parse the domainBlocks
  withBlocks(domainBlocks)
}

/** the simplest remote spec, from URL */
case class DieselUrlTextSpec (url:String, override val name:String)
    extends BaseTextSpec(name, Snakk.body(Snakk.url(url))) {
  override def mkParser = new DieselTextParser("rk")

  def + (other:DieselTextSpec) : DieselTextSpec = {
    DieselTextSpec(name+other.name, text+other.text)
  }
}

