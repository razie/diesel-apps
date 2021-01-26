/*   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package tests

import akka.actor.ActorSystem
import akka.util.Helpers.Requiring
import com.typesafe.config.ConfigFactory
import org.scalatest.Matchers.noException.should
import org.scalatest.{MustMatchers, OptionValues, WordSpecLike}
import razie.diesel.dom.WTypes
import razie.diesel.engine.nodes.StoryNode
import razie.diesel.engine.{DieselAppContext, DomAst, DomEngine, DomEngineSettings, DomEngineView}
import razie.diesel.expr.{AExprIdent, CExpr, SimpleExprParser}
import razie.diesel.samples.DomEngineUtils
import razie.wiki.parser.DieselUrlTextSpec

/**
  * run like: sbt 'testOnly *TestExprParser'
  */
class TestExprParser extends WordSpecLike with MustMatchers with OptionValues {

  //  final val SERVER = "http://specs.razie.com"
  final val SERVER = "http://localhost:9000"

  def p(s: String) = (new SimpleExprParser).parseExpr(s).get

  "CExpr parser" should {

    "parse numbers" in {
      assert(p("3").isInstanceOf[CExpr[_]])
      assert(p("3").getType == WTypes.NUMBER)
    }

    "parse strings" in {
      assert(p("\"a string\"").isInstanceOf[CExpr[_]])
      assert(p("\"a string\"").getType == WTypes.STRING)
    }
  }

  "id parser" should {

    "parse id" in {
      assert(p("anid").isInstanceOf[AExprIdent])
      assert(p("'an id'").isInstanceOf[AExprIdent])
      assert(p("anid[23]").isInstanceOf[AExprIdent])
      assert(p("anid[\"field1\"]").isInstanceOf[AExprIdent])
      assert(p("anid[\"field1\"][4]").isInstanceOf[AExprIdent])
      assert(p("anid[\"field1\"][4].jake.gg[4][\"asfd\"]").isInstanceOf[AExprIdent])
    }
  }

  // create an actor system or use the default etc
  implicit val system = ActorSystem("testsystem", ConfigFactory.parseString(""" """))

  // tell the engine to use this system
  DieselAppContext
      .withSimpleMode()
      .withActorSystem(system)

  var engine: DomEngine = null

  "epxr parser" should {

    "parse expr_story" in {

      val specs = List(
//        DieselUrlTextSpec(SERVER + "/wiki/Spec:Default_executors/included", "Default_executors"),
        DieselUrlTextSpec(SERVER + "/wiki/Spec:expr_spec/included", "expr_spec")
      )
      val story = DieselUrlTextSpec(SERVER + "/wiki/Story:expr_story/included", "expr_story")

      // run it: create engine, run story and wait for result
      engine =
          DomEngineUtils.mkEngine(
            new DomEngineSettings()
                .copy(realm = Some("rk"))
                .copy(mockMode = true)
                .copy(blenderMode = true),
            specs,
            List(story)
          )

      assert(engine.failedTestCount == 0)
    }
  }

  "engine" should {
    "run expr_story" in {
      // create an actor system or use the default etc

      // run it: create engine, run story and wait for result
      engine = DomEngineUtils.execAndWait(engine)

//      println(e2.root.toString)    // debug trace of engine's execution
//      println(e2.resultingValue)   // resulting value, if any


//      e2.root.children.collect {
//        case a: DomAst if a.value.isInstanceOf[StoryNode] => a.value.asInstanceOf[StoryNode]
//      }.foreach { story =>
//        it(s"should run story: ${story.path} ") {
//
//        }
//      }

      println("==================FAILURES==================")
      DomEngineView.failedTestListStr(List(engine.root)).map(println)
      println("==================FAILURES==================")

      assert(engine.failedTestCount == 0)
    }
  }
//
//  DomEngineView.storySummaryNice(engine.root).foreach { case (story, failed, total) =>
//    s"should run story: $story failed:$failed / total:$total" should {
//      "pass all tests" in {
//        assert(failed == 0)
//      }
//    }
//  }


}

