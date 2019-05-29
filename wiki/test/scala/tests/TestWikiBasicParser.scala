/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package scala.tests

import org.scalatest._
import org.scalatestplus.play.PlaySpec
import razie.tconf.BaseTextSpec
import razie.tconf.parser.JMapFoldingContext
import razie.wiki.model._
import razie.wiki.parser.{CsvParser, DomParser, WikiParserT}

// todo needs to go into tests

//class ParserSpec extends WordSpec with MustMatchers with OptionValues {
class TestWikiBasicParser extends PlaySpec {

  "parser" should {

    "parse md" in {

      val res = applys(
        """
some text
""")
      assert(res contains "some text")
    }
  }

  val tabsp1 =
    """
Ontario (slalom and GS), within the following four age groups:

- Nancy Greene League Racing (ages 7 to 10)
- K1 League Racing (ages 11 and 12),
- K2 League Racing (ages 13 and 14) and
- J Alpine League Racing (ages 15 to 18)
"""
  //  println (WikiParser.applys(tabsp))
  //  println (Wikis.format(WID.NONE, "md", tabsp))
  //  val content = Wikis.preprocess(WID.NONE, "md", tabsp1).s
  val content =
  """fufu

 """
  //  println(toXHTML(knockoff(content)).toString)
  //  println(Wikis.format (WID("1","2"), "md", content))
  //  println(Wikis.format(WID("1", "2"), "md", content))

  //---------------------

  //  println(f())

  def f() = {
    val ccc =
      """
{{section:supportreq}}
present
{{/section}}
"""

  }

  /** the simplest spec - from a named string property */
  case class XTextSpec (override val name:String, override val text:String) extends BaseTextSpec(name, text) {
    override def mkParser = new XTextParser("rk")
  }

  class XTextParser(val realm: String) extends WikiParserT with DomParser {
    withBlocks(domainBlocks)
  }

  def applys(s: String) = new XTextSpec("spec", s).parsed

}

