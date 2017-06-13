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
  final val RECEIVED = "received"
  final val SAMPLED = "sampled"
  final val GENERATED = "generated"
  final val SUBTRACE = "subtrace"
  final val RULE = "rule"
  final val SKETCHED = "sketched"
  final val MOCKED = "mocked"
  final val TEST = "test"
  final val NEXT = "next"

  def isGenerated  (k:String) = GENERATED==k || SKETCHED==k || MOCKED==k
  def shouldIgnore (k:String) = RULE==k
  def shouldSkip (k:String) = NEXT==k
}

object DomState {
  final val INIT="final.init" // new node
  final val STARTED="exec.started" // is executing now
  final val DONE="final.done" // done
  final val LATER="exec.later" // queued up somewhere for later
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
  value: Any,
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

  /** recursive tostring */
  private def tos(level: Int, html:Boolean): String = {
    def toschildren (level:Int, kids : List[DomAst]) : List[Any] =
      kids.filter(k=> !AstKinds.shouldIgnore(k.kind)).flatMap{k=>
        if(false && AstKinds.shouldSkip(k.kind)) {
          toschildren(level+1, k.children.toList)
        } else
          List(k.tos(level+1, html))
      }

    ("  " * level) + kind + "::" + {
      value match {
        case c:CanHtml if(html) => c.toHtml
        case x => x.toString
      }
    }.lines.map(("  " * level) + _).mkString("\n") + moreDetails + "\n" +
      //    children.filter(k=> !AstKinds.shouldIgnore(k.kind)).map(_.tos(level + 1, html)).mkString
      toschildren(level, children.toList).mkString
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
        if(AstKinds.shouldSkip(k.kind)) {
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


