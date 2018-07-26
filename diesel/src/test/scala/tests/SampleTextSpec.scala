package tests

import razie.tconf.BaseTextSpec
import razie.wiki.parser.{DomParser, ExprParser, SimpleSpecParser}

/** the simplest spec - uses the Sample parser we created here */
case class SampleTextSpec (override val name:String, override val text:String) extends BaseTextSpec(name, text) {
  override def mkParser = new SampleTextParser("rk")
}

/** A simple parser for our simple specs
  *
  * DomParser is the actual Diesel/Dom parser.
  * We extend from it to include its functionality and then we add its parsing rules with withBlocks()
  */
class SampleTextParser(val realm: String) extends SimpleSpecParser with DomParser {
  // include the rules to parse the domainBlocks
  withBlocks(domainBlocks)
}


