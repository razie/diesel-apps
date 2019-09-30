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
  var kind: String = AstKinds.GENERATED,
  children: ListBuffer[DomAst] = new ListBuffer[DomAst](),
  id : String = new ObjectId().toString
  ) extends CanHtml {

  private var istatus:String = DomState.INIT
  def status:String = istatus
  def status_=(s:String) = istatus = s

  /** timestamp started */
  var tstart:Long = System.currentTimeMillis()
  /** timestamp started */
  var tend:Long = System.currentTimeMillis()
  /** execution sequence number - an engine is a single sequence */
  var seqNo:Long = -1

  def appendAll(other:List[DomAst])(implicit engine: DomEngine) = {
    engine.evAppChildren(this, other)
  }

  def append(other:DomAst)(implicit engine: DomEngine) = {
    engine.evAppChildren(this, other)
  }

  def start(seq:Long) = {
    tstart = System.currentTimeMillis()
    seqNo = seq
  }

  def end = {
    tend = System.currentTimeMillis()
  }

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

  /** non-recursive tostring */
  def meTos(level: Int, html:Boolean): String = {

    def theKind = {
      val duration = tend - tstart
      if(html) s"""<span seqNo="$seqNo" msec="$duration" id="$id" prereq="${prereq.mkString(",")}" title="$kind, $duration ms">${kind.take(3)}</span>"""
      else kind
    }

    (" " * level) +
    theKind +
    "::" + {
    value match {
      case c: CanHtml if (html) => c.toHtml
      case x => x.toString
    }
    }.lines.map((" " * 1) + _).mkString("\n") + moreDetails
  }

  /** recursive tostring */
  private def tos(level: Int, html:Boolean): String = {

    def h(s:String) = if(html) s else ""

    def toschildren (level:Int, kids : List[DomAst]) : List[Any] =
      kids.filter(k=> !shouldIgnore(k)).flatMap{k=>
        if(shouldRollup(k) && k.children.size == 1) {
//           rollup NEXT nodes and others - just show the children
          toschildren(level+1, k.children.toList)
        } else
          List(k.tos(level+1, html))
      }

    if(!shouldSkip(this)) {
        h(s"""<div kind="$kind" level="$level">""") +
        meTos(level, html) + "\n" +
        toschildren(level, children.toList).mkString +
        h("</div>")
    } else {
      toschildren(level, children.toList).mkString
    }
  }

  override def toString = tos(0, false)

  /** this html works well in a diesel fiddle, use toHtmlInPage elsewhere */
  override def toHtml = tos(0, true)
  def toHtml (level : Int) = tos(level, true)

  /** as opposed to toHtml, this will produce an html that can be displayed in any page, not just the fiddle */
  def toHtmlInPage = toHtml.replaceAllLiterally("weref", "wefiddle")

  type HasJ = {def toj : Map[String,Any]}

  def toj : Map[String,Any] = {
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

  // visit/recurse with filter AND level
  def collect2[T](f: PartialFunction[(DomAst, Int), T]) : List[T] = {
    val res = new ListBuffer[T]()

    def inspect(d: DomAst, level: Int): Unit = {
      if (f.isDefinedAt((d, level))) res append f((d, level))
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
    if(this.id == id)
      Some(this)
    else
      children.foldLeft(None:Option[DomAst])((a,b)=>a orElse b.find(id))

  def setKinds (kkk:String) : DomAst = {
    this.kind=kkk
    this.children.map(_.setKinds(kkk))
    this
  }
}
