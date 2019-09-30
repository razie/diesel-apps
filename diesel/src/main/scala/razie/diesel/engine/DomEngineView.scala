/**
  *  ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.engine

import org.bson.types.ObjectId
import razie.diesel.engine.RDExt.TestResult
import razie.diesel.ext._
import razie.diesel.ext.EnginePrep.StoryNode
import scala.collection.mutable.ListBuffer
import razie.diesel.utils.DomHtml.quickBadge

/** a tree node
  *
  * kind is spec/sampled/generated/test etc
  *
  * todo optimize tree structure, tree binding
  *
  * todo need ID conventions to suit distributed services
  */
object DomEngineView {

  /** story summary if this was a test story */
  def storySummary (a:DomAst) = {
    val zip = a.children.zipWithIndex
    val stories = zip.filter(_._1.kind == "story")

    val resl = Range(0, stories.size).map { i =>
      val s = stories(i)
      val nodes =
        if(i < stories.size - 1) a.children.slice(s._2+1, stories(i+1)._2 - 1)
        else a.children.slice(s._2+1, a.children.size - 1)

      val failed = failedTestCount(nodes.toList)
      val total = totalTestCount(nodes.toList)

      s"""<a href="#${s._1.value.asInstanceOf[StoryNode].path.wpath.replaceAll("^.*:", "")}">[details]</a>""" +
      quickBadge(failed, total, -1, "") +
        (s._1.value match {
            // doing this to avoid getting the a name at the top and confuse the scrolling
        case sn : StoryNode => s""" Story ${sn.path.ahref.mkString}"""
        case _ => s._1.meTos(1, true)
      })
    }

    resl.mkString("\n")
  }

  // failed tests
  def failedTestCount(a:DomAst): Int = failedTestCount(List(a))

  // exceptions and errors other than failed tests
  def errorCount(a:DomAst): Int = errorCount(List(a))

  def successTestCount(a:DomAst) : Int = successTestCount(List(a))

  def totalTestCount(a:DomAst) : Int = totalTestCount(List(a))

  def todoTestCount(a:DomAst) : Int = todoTestCount(List(a))

  /** count test results */
  def totalTestCount(nodes:List[DomAst]): Int = (nodes.flatMap(_.collect {
    case d@DomAst(n: TestResult, _, _, _) => n
  })).size

  /** count tests to do */
  def todoTestCount(nodes:List[DomAst]): Int = (nodes.flatMap(_.collect {
    case d@DomAst(n: ExpectAssert, _, _, _) /*if DomState.isDone(d.status)*/ => n
    case d@DomAst(n: ExpectM, _, _, _) /*if DomState.isDone(d.status)*/ => n
    case d@DomAst(n: ExpectV, _, _, _) /*if DomState.isDone(d.status)*/ => n
  })).size

  def failedTestCount(nodes:List[DomAst]): Int = (nodes.flatMap(_.collect {
    case d@DomAst(n: TestResult, _, _, _) if n.value.startsWith("fail") => n
    case d@DomAst(n: EError, _, _, _) => n
  })).size

  def errorCount(nodes:List[DomAst]): Int = (nodes.flatMap(_.collect {
    case d@DomAst(n: EError, _, _, _) => n
  })).size

  def successTestCount (nodes:List[DomAst]): Int = (nodes.flatMap(_.collect {
    case d@DomAst(n: TestResult, _, _, _) if n.value.startsWith("ok") => n
  })).size
}
