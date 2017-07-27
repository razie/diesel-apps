package tests

import razie.diesel.dom._
import razie.diesel.engine.DomEngineSettings
import razie.diesel.samples.DomEngineUtils
import org.scalatestplus.play._
import razie.wiki.parser.WAST.JMapFoldingContext
import razie.wiki.parser.{WikiDomParser, WikiParserT}
import play.api.{Play, Application}
import play.api.inject.guice._


/**
  * Created by raz on 2017-07-06.
  */
//class SimpleSpec extends FlatSpec {
class SimpleSpec extends PlaySpec /*with GuiceOneAppPerSuite */ {

  val application: Application = new GuiceApplicationBuilder()
    .configure("some.configuration" -> "value")
    .build()
  Play.start(application)

  "simple specs" should {
    "work ok" in {
      assert(1 == 1)
    }

    "concatenate specs" in {
      val engine = DomEngineUtils.execAndWait(DomEngineUtils.mkEngine(new DomEngineSettings(), specs, List(story)))
      println(engine.root.toString)
      println(engine.resultingValue)
      assert(engine.resultingValue contains "Jane")
    }
  }


  val specs = List(
    XTextSpec("spec1",
      """
$when home.guest_arrived(name) => lights.on
$when home.guest_arrived(name == "Jane") => chimes.welcome(name)
""".stripMargin
    ),

    XTextSpec("spec2",
      """
$when chimes.welcome(name) => (greeting = "Greetings, "+name)
""".stripMargin
    )
  ).map(_.xparsed)

  // some trigger message
  val story =
    XTextSpec("story1",
      """
$send home.guest_arrived(name="Jane")
""".stripMargin
    ).xparsed
}

/** the simplest spec - from a named string property */
case class XTextSpec(val name: String, val text: String) extends DSpec {
  def specPath: TSpecPath = new SpecPath("local", name, "")

  def findTemplate(name: String, direction: String = ""): Option[DTemplate] = None

  /** other parsing artifacts to be used by knowledgeable modules.
    * Parsers can put stuff in here. */
  val cache = new scala.collection.mutable.HashMap[String, Any]()

  /** the assumption is that specs can parse themselves and cache the AST elements
    *
    * errors must contain "CANNOT PARSE" and more information
    *
    * todo parsed should be an Either
    */
  private var iparsed: Option[XTextSpec] = None
  private var sparsed: Option[String] = None

  // parse just once
  def parsed: String = sparsed.getOrElse {
    val res = {
      val p = new Parser("rk")
      p.apply(text).fold(new JMapFoldingContext(Some(this), None)).s
    }
    sparsed = Some(res)
    iparsed = Some(this)
    res
  }

  def xparsed: XTextSpec = iparsed.getOrElse {
    parsed
    iparsed.get
  }


  class Parser(val realm: String) extends WikiParserT with WikiDomParser {
    withBlocks(domainBlocks)
  }


}

