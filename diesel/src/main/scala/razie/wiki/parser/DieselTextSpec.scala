/**
  *   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.wiki.parser

import razie.tconf.BaseTextSpec

/** the simplest spec used in samples and tests - uses the Diesel-Dom parser */
case class DieselTextSpec (override val name:String, override val text:String) extends BaseTextSpec(name, text) {
  override def mkParser = new DieselTextParser("rk")
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

