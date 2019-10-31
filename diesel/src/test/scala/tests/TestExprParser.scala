/*   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package tests

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, MustMatchers, OptionValues, WordSpecLike}
import razie.diesel.dom.WTypes
import razie.diesel.engine.{DieselAppContext, DomEngineSettings}
import razie.diesel.expr.{AExprIdent, CExpr, ExprParser, SimpleExprParser}
import razie.diesel.samples.DomEngineUtils
import razie.wiki.parser.DieselUrlTextSpec

/**
  * run like: sbt 'testOnly *TestExprParser'
  */
class TestExprParser extends WordSpecLike with MustMatchers with OptionValues {

  def p(s:String) = (new SimpleExprParser).parseExpr(s).get

  "CExpr parser" should {

    "parse numbers" in {
      assert(p( "3" ).isInstanceOf[CExpr[_]])
      assert(p( "3" ).getType == WTypes.NUMBER)
    }

    "parse strings" in {
      assert(p( "\"a string\"" ).isInstanceOf[CExpr[_]])
      assert(p( "\"a string\"" ).getType == WTypes.STRING)
    }
  }

  "id parser" should {

    "parse id" in {
      assert(p( "anid" ).isInstanceOf[AExprIdent])
      assert(p( "'an id'" ).isInstanceOf[AExprIdent])
      assert(p( "anid[23]" ).isInstanceOf[AExprIdent])
      assert(p( "anid[\"field1\"]" ).isInstanceOf[AExprIdent])
      assert(p( "anid[\"field1\"][4]" ).isInstanceOf[AExprIdent])
      assert(p( "anid[\"field1\"][4].jake.gg[4][\"asfd\"]" ).isInstanceOf[AExprIdent])
    }
  }

  "epxr parser" should {

    "parse all samples" in {
      // create an actor system or use the default etc

      implicit val system = ActorSystem("testsystem", ConfigFactory.parseString(""" """))

      // tell the engine to use this system
      DieselAppContext
          .withSimpleMode()
          .withActorSystem(system)

      val spec  = DieselUrlTextSpec ("http://specs.razie.com/wiki/Story:expr_spec/included", "expr_spec")
      val story = DieselUrlTextSpec ("http://specs.razie.com/wiki/Story:expr_story/included", "expr_story")

      // run it: create engine, run story and wait for result
      val engine = DomEngineUtils.execAndWait(
        DomEngineUtils.mkEngine(
          new DomEngineSettings().copy(realm=Some("rk")),
          List(spec),
          List(story)
        )
      )

      println(engine.root.toString)    // debug trace of engine's execution
      println(engine.resultingValue)   // resulting value, if any

      assert(engine.failedTestCount == 0)
    }
  }
}

