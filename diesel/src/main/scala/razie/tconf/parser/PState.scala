/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.tconf.parser

import razie.tconf.{DSpec, DUser}
import razie.{audit, cdebug}


/** folding context base class - page being parsed and for which user */
abstract class FoldingContext[+T <: DSpec, +U <: DUser] {
  def we:Option[T]
  def au:Option[U]
  def target:String        // why is this being parsed? template, view or pre-processed

  /** evaluate expressions in the folded content
    * kind can be "$" or "$$"
    *
    * Expressions can be simple parm access like $name OR api.wix
    */
  def eval (kind:String, expr:String) : String

  def wpath = we.map(_.specPath.wpath)

  def cacheable:Boolean = we.map(_.cacheable).getOrElse(false)
  def cacheable_= (v:Boolean) = we.foreach(_.cacheable = v)
}

/** folding context using a map for the properties available to evaluate expressions */
class JMapFoldingContext[T <: DSpec, U <: DUser] (
  val we:Option[T],
  val au:Option[U],
  val target:String = T_VIEW,
  val ctx:Map[String,Any]=Map.empty
  ) extends FoldingContext[T,U] {

  /** kind can be "$" or "$$" */
  def eval (kind:String, expr:String) : String =
    (if(kind == "$$" && target == T_TEMPLATE || kind == "$") ex(ctx, expr.split("\\.")) else None) getOrElse s"`{{$kind$expr}}`"

  /** resolve a name a.b.c in a given context */
  private def ex (m:Map[String, Any], terms:Array[String]) : Option[String] =
    if(terms.size > 0 && m.contains(terms(0))) m(terms(0)) match {
      case m: Map[_, _] if terms.size>1 => ex(m.asInstanceOf[Map[String,Any]], terms.drop(1))
      case s: String => Some(s)
      case l: List[_] => Some(l.mkString)
      case h @ _ => Some(h.toString)
    } else None
}

/** an AST node collects the result of a parser rule - base trait for all AST node types
  *
  * @param s the HTML representation of this section
  * @param tags the properties collected from this section, will be added to the page's tags
  * @param ilinks the links to other pages collected in this rule
  */
trait PState {
  def s: String
  def props: Map[String, String]
  def ilinks: List[Any]

  /** composing AST elements */
  def + (other: PState) : PState = LState (this, other)

  /** lazy unfolding of AST tree */
  def fold(ctx:FoldingContext[_,_]) : SState = {
    try {
      if (ParserSettings.debugStates) {
        cdebug << "======================= F O L D ========================="
        cdebug << print(1)
        cdebug << "======================= F O L D I N G ========================="
      }
      ifold(SState.EMPTY, ctx)
    } catch {
      case t: Throwable =>
        audit.Audit.logdb("EXCEPTION_PARSING.folding - " + ctx.wpath + " " + t.getMessage)
        razie.Log.error("EXCEPTION_PARSING.folding - " + ctx.wpath + " ", t)
        SState("EXCEPTION_PARSING.folding - " + t.getLocalizedMessage())
    }
  }
  def ifold(current:SState, ctx:FoldingContext[_,_]) : SState

  def print (level:Int) : String = ("--" * level) + this.toString
  def printHtml (level:Int) : String = "<ul>"+this.toString+"</ul>"
}

object SState {
  // todo why can't this be a final val ?
  def EMPTY = SState("")
}

/** leaf AST node - final computed value
  */
case class OState(s: String, other:PState, props: Map[String, String] = Map.empty, ilinks: List[Any] = List.empty) extends PState {
  if (ParserSettings.debugStates) cdebug << this.toString

  override def ifold(current:SState, ctx:FoldingContext[_,_]) : SState =
    other.ifold(current, ctx).copy(s = s)

  override def toString = s"SSTATE ($s)"
}

/** leaf AST node - final computed value
  *
  * @param s the string representation of this section
  * @param props the tags collected from this section, will be added to the page's tags
  * @param ilinks the links to other pages collected in this rule
  */
case class SState(s: String, props: Map[String, String] = Map.empty, ilinks: List[Any] = List.empty) extends PState {
  if (ParserSettings.debugStates) cdebug << this.toString

  def this(s: String, ilinks: List[Any]) = this(s, Map.empty, ilinks)

  override def ifold(current:SState, ctx:FoldingContext[_,_]) : SState = this
  override def toString = s"SSTATE ($s)"
}

/** a list of AST nodes - it's a branch in the AST tree */
case class LState(states:PState*) extends PState {
  def this(l:List[PState]) = this ()

  if (ParserSettings.debugStates) cdebug << this.toString

  // nobody should ask for these - fold the parse result into a SState always
  override def s: String = ???
  override def props: Map[String, String] = ???
  override def ilinks: List[Any] = ???

  /** optimize composiiotn of lists */
  override def +(other: PState) : PState = other match {
    case ls:LState => LState(states ++ ls.states)
    case ps:PState => LState(states ++ Seq(ps))
  }

  override def ifold(current:SState, ctx:FoldingContext[_,_]) : SState = {
    states.foldLeft(SState.EMPTY) {(a,b)=>
      val c = b.ifold(a, ctx)
      SState(a.s + c.s, a.props ++ c.props, a.ilinks ++ c.ilinks)
    }
  }
  override def toString = s"LSTATE (${states.mkString})"
  override def print (level:Int):String = ("--" * level) + "LState" + states.map (_.print(level+1)).mkString
  override def printHtml (level:Int):String = "LState" + "<ul>"+states.map (x=>"<li>"+x.printHtml(level+1)).mkString + "</ul>"
}

/** an aggregation AST node - pattern is: prefix+midAST+suffix */
case class RState(prefix:String, mid:PState, suffix:String)  extends PState {
  if (ParserSettings.debugStates) cdebug << this.toString

  // nobody should ask for these - fold the parse result into a SState always
  override def s: String = ???
  override def props: Map[String, String] = ???
  override def ilinks: List[Any] = ???

  override def ifold(current:SState, ctx:FoldingContext[_,_]) : SState = {
    val c = mid.ifold(current, ctx)
    SState(prefix + c.s + suffix, c.props, c.ilinks)
  }
  override def toString = s"RSTATE ($prefix, $mid, $suffix)"
  override def print (level:Int):String = ("--" * level) + s"RSTATE ($prefix, $suffix)" +  mid.print(level+1)
  override def printHtml (level:Int):String = s"RSTATE ($prefix, $suffix)" +  "<ul><li>"+mid.printHtml(level+1)+"</ul>"
}

/** lazy AST node - value computed when they're folded.
  *
  * By default a lazy state will cause a non cacheable wiki
  */
case class LazyState[T <: DSpec, U <: DUser] (f:(SState, FoldingContext[T,U]) => SState) extends PState {
  var dirty = true

  if (ParserSettings.debugStates) cdebug << this.toString

  // nobody should ask for these - fold the parse result into a SState always
  override def s: String = ???
  override def props: Map[String, String] = ???
  override def ilinks: List[Any] = ???

  override def ifold(current:SState, ctx:FoldingContext[_,_]) : SState = {
    if(dirty) ctx.cacheable = false
    f(current, ctx.asInstanceOf[FoldingContext[T,U]])
  }
  override def toString =  s"LazySTATE ()"

  def cacheOk = {this.dirty=false; this}
}



