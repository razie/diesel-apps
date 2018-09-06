/**
 *  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.dom

import org.bson.types.ObjectId
import razie.diesel.ext._

import scala.collection.mutable.ListBuffer

/** the kinds of nodes we understand */
object AstKinds {
  final val ROOT = "root"
  final val STORY = "story"
  final val RECEIVED = "received"
  final val SAMPLED = "sampled"
  final val DEBUG = "debug"
  final val GENERATED = "generated"
  final val SUBTRACE = "subtrace"
  final val RULE = "rule"
  final val SKETCHED = "sketched"
  final val MOCKED = "mocked"
  final val TEST = "test"
  final val BUILTIN = "built-in"
  final val NEXT = "next"

  def isGenerated  (k:String) = GENERATED==k || SKETCHED==k || MOCKED==k || DEBUG==k
  def shouldIgnore (k:String) = RULE==k || BUILTIN==k
  def shouldSkip (k:String) = NEXT==k
  def shouldRollup (k:String) = NEXT==k
  def shouldPrune (k:String) = false
}

/** mix this in if you want to control display/traversal */
trait DomAstInfo {
  /** prune i.e. stop showing children */
  def shouldPrune : Boolean

  /** ignore this node and branch */
  def shouldIgnore : Boolean

  /** skip this node */
  def shouldSkip : Boolean

  /** don't show this node, just show it's children as if they'r eunder the parent */
  def shouldRollup : Boolean
}

object DomState {
  final val INIT="final.init" // new node
  final val STARTED="exec.started" // is executing now
  final val DONE="final.done" // done
  final val LATER="exec.later" // queued up somewhere for later
  final val ASYNC="exec.async" // started but will complete itself later
  final val DEPENDENT="exec.depy" // waiting on another task

  def inProgress(s:String) = s startsWith "exec."
}

/** a tree node
  *
  * kind is spec/sampled/generated/test etc
  *
  * todo optimize tree structure, tree binding
  *
  * todo need ID conventions to suit distributed services
  */
case class DomAst(
  var value: Any,
  kind: String = AstKinds.GENERATED,
  children: ListBuffer[DomAst] = new ListBuffer[DomAst](),
  id : String = new ObjectId().toString
  ) extends CanHtml {

  private var istatus:String = DomState.INIT
  def status:String = istatus
  def status_=(s:String) = istatus = s

  var moreDetails = " "
  var specs: List[Any] = Nil
  var prereq: List[String] = Nil

  /** depends on other nodes by IDs */
  def withPrereq (s:List[String]) = {
    prereq = s ::: prereq
    this
  }

  /** this node has a spec */
  def withSpec (s:Any) = {
    specs = s :: specs
    this
  }

  def withStatus (s:String) = {
    this.status=s
    this
  }

  def withDetails (s:String) = {
    moreDetails = moreDetails + s
    this
  }

  private def shouldPrune (k:DomAst) =
    AstKinds.shouldPrune(k.kind) ||
      k.value.isInstanceOf[DomAstInfo] && k.value.asInstanceOf[DomAstInfo].shouldPrune

  private def shouldIgnore (k:DomAst) =
    AstKinds.shouldIgnore(k.kind) ||
      k.value.isInstanceOf[DomAstInfo] && k.value.asInstanceOf[DomAstInfo].shouldIgnore

  private def shouldSkip (k:DomAst) =
    AstKinds.shouldSkip(k.kind) ||
      k.value.isInstanceOf[DomAstInfo] && k.value.asInstanceOf[DomAstInfo].shouldSkip

  private def shouldRollup (k:DomAst) =
    AstKinds.shouldRollup(k.kind) ||
      k.value.isInstanceOf[DomAstInfo] && k.value.asInstanceOf[DomAstInfo].shouldRollup

  /** recursive tostring */
  private def tos(level: Int, html:Boolean): String = {

    def theKind =
      if(html) s"""<span title="$kind">${kind.take(3)}</span>"""
      else kind

    def toschildren (level:Int, kids : List[DomAst]) : List[Any] =
      kids.filter(k=> !shouldIgnore(k)).flatMap{k=>
        if(shouldRollup(k) && k.children.size == 1) {
//           rollup NEXT nodes and others - just show the children
          toschildren(level+1, k.children.toList)
        } else
          List(k.tos(level+1, html))
      }

    if(!shouldSkip(this)) {
      s"""<div kind="$kind" level="$level">""" +
        (" " * level) +
        theKind +
        "::" + {
        value match {
          case c: CanHtml if (html) => c.toHtml
          case x => x.toString
        }
      }.lines.map((" " * 1) + _).mkString("\n") + moreDetails + "\n" +
        toschildren(level, children.toList).mkString +
        "</div>"
    } else {
      toschildren(level, children.toList).mkString
    }
  }


  override def toString = tos(0, false)

  /** this html works well in a diesel fiddle, use toHtmlInPage elsewhere */
  override def toHtml = tos(0, true)

  /** as opposed to toHtml, this will produce an html that can be displayed in any page, not just the fiddle */
  def toHtmlInPage = toHtml.replaceAllLiterally("weref", "wefiddle")

  type HasJ = {def toj : Map[String,Any]}

  def toj : Map[String,Any] = {
    razie.clog << "VALUE CLASS "+value.getClass.getSimpleName
    Map (
      "class" -> "DomAst",
      "kind" -> kind,
      "value" ->
        (value match {
          case m if m.getClass.getDeclaredMethods.exists(_.getName == "toj") => m.asInstanceOf[HasJ].toj
          case x => x.toString
        }),
      "details" -> moreDetails,
      "id" -> id,
      "status" -> status,
      "children" -> tojchildren(children.toList)
    )
  }

  def tojchildren (kids : List[DomAst]) : List[Any] =
      kids.filter(k=> !AstKinds.shouldIgnore(k.kind)).flatMap{k=>
        if(shouldSkip(k)) {
          tojchildren(k.children.toList)
        } else
          List(k.toj)
      }

  def toJson = toj

  // visit/recurse with filter
  def collect[T](f: PartialFunction[DomAst, T]) : List[T] = {
    val res = new ListBuffer[T]()
    def inspect(d: DomAst, level: Int): Unit = {
      if (f.isDefinedAt(d)) res append f(d)
      d.children.map(inspect(_, level + 1))
    }
    inspect(this, 0)
    res.toList
  }

  /** GUI needs position info for surfing */
  def posInfo = collect{
    case d@DomAst(m:EMsg, _, _, _) if(m.pos.nonEmpty) =>
      Map(
        "kind" -> "msg",
        "id" -> (m.entity+"."+m.met),
        "pos" -> m.pos.get.toJmap
      )
  }

  /** find in subtree, by id */
  def find(id:String) : Option[DomAst] =
    if(this.id == id) Some(this)
    else children.foldLeft(None:Option[DomAst])((a,b)=>a orElse b.find(id))
}


