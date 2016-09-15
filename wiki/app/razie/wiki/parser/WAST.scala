/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.parser

import razie.cdebug
import razie.wiki.model.{WikiUser, ILink, WikiEntry}

/**
 * wiki AST - abstract syntax tree.
 * 1. Wikis are parsed in AST
 * 2. then folded into markdown with a context
 * 3. then markdown is turned into html via markdown parser
 */
object WAST {
  final val T_PREPROCESS = "pre"
  final val T_VIEW = "view"
  final val T_TEMPLATE = "template"

  private def toMap (we:Option[WikiEntry]) = we.map(w=>Map(
    "category" -> w.category,
    "name" -> w.name,
    "wpath"->w.wid.wpath
  )).getOrElse(Map.empty)

  /** create a normal context - same as VIEW */
  def context         (we:Option[WikiEntry], au:Option[WikiUser]=None, ctx:Map[String,Any]=Map.empty) =
    new JMapFoldingContext(we, au, T_PREPROCESS, ctx ++ toMap(we))
  /** create a context for view */
  def contextView     (we:Option[WikiEntry], au:Option[WikiUser]=None, ctx:Map[String,Any]=Map.empty) =
    new JMapFoldingContext(we, au, T_VIEW, ctx ++ toMap(we))
  /** create a special context for templates - template expressions are expanded only */
  def contextTemplate (we:Option[WikiEntry], au:Option[WikiUser]=None, ctx:Map[String,Any]=Map.empty) =
    new JMapFoldingContext(we, au, T_TEMPLATE, ctx ++ toMap(we))

  /** folding context base class */
  abstract class FoldingContext {
    def we:Option[WikiEntry]
    def au:Option[WikiUser]
    def target:String

    /** evaluate expressions in the folded content
      * kind can be "$" or "$$"
      *
      * Expressions can be simple parm access like $name OR api.wix
      */
    def eval (kind:String, expr:String) : String
  }

  /** folding context using a map for the properties available to evaluate expressions */
  class JMapFoldingContext (val we:Option[WikiEntry], val au:Option[WikiUser], val target:String = T_VIEW, val ctx:Map[String,Any]=Map.empty) extends FoldingContext {
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
    def ilinks: List[ILink]

    /** composing AST elements */
    def + (other: PState) : PState = LState (this, other)

    /** lazy unfolding of AST tree */
    def fold(ctx:FoldingContext) : SState = {
      if(ParserSettings.debugStates) {
        cdebug << "======================= F O L D ========================="
        cdebug << print (1)
        cdebug << "======================= F O L D I N G ========================="
      }
      ifold (SState.EMPTY, ctx)
    }
    def ifold(current:SState, ctx:FoldingContext) : SState

    def print (level:Int) : String = ("--" * level) + this.toString
    def printHtml (level:Int) : String = "<ul>"+this.toString+"</ul>"
  }

  object SState {
    // todo why can't this be a final val ?
    def EMPTY = SState("")
  }

  /** leaf AST node - final computed value
    *
    * @param s the string representation of this section
    * @param props the tags collected from this section, will be added to the page's tags
    * @param ilinks the links to other pages collected in this rule
    */
  case class SState(s: String, props: Map[String, String] = Map.empty, ilinks: List[ILink] = List.empty) extends PState {
    if (ParserSettings.debugStates) cdebug << this.toString

    def this(s: String, ilinks: List[ILink]) = this(s, Map.empty, ilinks)

    override def ifold(current:SState, ctx:FoldingContext) : SState = this
    override def toString = s"SSTATE ($s)"
  }

  /** a list of AST nodes - it's a branch in the AST tree */
  case class LState(states:PState*) extends PState {
    def this(l:List[PState]) = this ()

    if (ParserSettings.debugStates) cdebug << this.toString

    override def s: String = ???
    override def props: Map[String, String] = ???
    override def ilinks: List[ILink] = ???

    /** optimize composiiotn of lists */
    override def +(other: PState) : PState = other match {
      case ls:LState => LState(states ++ ls.states)
      case ps:PState => LState(states ++ Seq(ps))
    }

    override def ifold(current:SState, ctx:FoldingContext) : SState = {
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

    override def s: String = ???
    override def props: Map[String, String] = ???
    override def ilinks: List[ILink] = ???

    override def ifold(current:SState, ctx:FoldingContext) : SState = {
      val c = mid.ifold(current, ctx)
      SState(prefix + c.s + suffix, c.props, c.ilinks)
    }
    override def toString = s"RSTATE ($prefix, $mid, $suffix)"
    override def print (level:Int):String = ("--" * level) + s"RSTATE ($prefix, $suffix)" +  mid.print(level+1)
    override def printHtml (level:Int):String = s"RSTATE ($prefix, $suffix)" +  "<ul><li>"+mid.printHtml(level+1)+"</ul>"
  }

  /** lazy AST node - value computed when they're folded */
  case class LazyState(f:(SState, FoldingContext) => SState)  extends PState {
    if (ParserSettings.debugStates) cdebug << this.toString

    override def s: String = ???
    override def props: Map[String, String] = ???
    override def ilinks: List[ILink] = ???

    override def ifold(current:SState, ctx:FoldingContext) : SState = f(current, ctx)
    override def toString =  s"LazySTATE ()"
  }

  implicit def toSState(s: String) : PState = SState(s)
  implicit def toLState(s: Seq[PState]) : PState = s match {
    // optimixze empty lists away
    case x :: Nil if (x.isInstanceOf[LState]) => x.asInstanceOf[LState]
    case x :: Nil  => x
    case _ => LState (s: _*)
  }
}
