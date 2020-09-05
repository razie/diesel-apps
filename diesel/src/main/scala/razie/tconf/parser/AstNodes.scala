/**
  *   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 **/
package razie.tconf.parser

import razie.tconf.{DSpec, DUser}
import razie.{audit, cdebug}

/** an AST node collects the result of a parser rule - base trait for all AST node types
  *
  * @param s the HTML representation of this section
  * @param tags the properties collected from this section, will be added to the page's tags
  * @param ilinks the links to other pages collected in this rule
  */
trait BaseAstNode {
  def s: String
  def props: Map[String, String]
  def ilinks: List[Any]

  /** composing AST elements */
  def +(other: BaseAstNode): BaseAstNode = ListAstNode(this, other)

  /** lazy unfolding of AST tree */
  def fold(ctx: FoldingContext[_, _]): StrAstNode = {
    try {
      if (SpecParserSettings.debugAstNodes) {
        cdebug << "======================= F O L D ========================="
        cdebug << print(1)
        cdebug << "======================= F O L D I N G ========================="
      }
      ifold(StrAstNode.EMPTY, ctx)
    } catch {
      case t: Throwable =>
        audit.Audit.logdb(
          "EXCEPTION_PARSING.folding", ctx.wpath + " " + t.getMessage
        )
        razie.Log.error("EXCEPTION_PARSING.folding - " + ctx.wpath + " ", t)
        StrAstNode("EXCEPTION_PARSING.folding - " + t.getLocalizedMessage())
    }
  }
  def ifold(current: StrAstNode, ctx: FoldingContext[_, _]): StrAstNode

  def print(level: Int): String = ("--" * level) + this.toString
  def printHtml(level: Int): String = "<ul>" + this.toString + "</ul>"
}

object StrAstNode {
  // todo why can't this be a final val ?
  def EMPTY = StrAstNode("")
}

/** leaf AST node - final computed value
  */
case class LeafAstNode(s: String,
                       other: BaseAstNode,
                       props: Map[String, String] = Map.empty,
                       ilinks: List[Any] = List.empty)
    extends BaseAstNode {
  if (SpecParserSettings.debugAstNodes) cdebug << this.toString

  override def ifold(current: StrAstNode,
                     ctx: FoldingContext[_, _]): StrAstNode =
    other.ifold(current, ctx).copy(s = s)

  override def toString = s"SSTATE ($s)"
}

/** leaf AST node - final computed value
  *
  * @param s the string representation of this section
  * @param props the tags collected from this section, will be added to the page's tags
  * @param ilinks the links to other pages collected in this rule
  */
case class StrAstNode(s: String,
                      props: Map[String, String] = Map.empty,
                      ilinks: List[Any] = List.empty)
    extends BaseAstNode {
  if (SpecParserSettings.debugAstNodes) cdebug << this.toString

  def this(s: String, ilinks: List[Any]) = this(s, Map.empty, ilinks)

  override def ifold(current: StrAstNode,
                     ctx: FoldingContext[_, _]): StrAstNode = this
  override def toString = s"SSTATE ($s)"
}

/** a list of AST nodes - it's a branch in the AST tree */
case class ListAstNode(states: BaseAstNode*) extends BaseAstNode {
  def this(l: List[BaseAstNode]) = this()

  if (SpecParserSettings.debugAstNodes) cdebug << this.toString

  // nobody should ask for these - fold the parse result into a SState always
  override def s: String = ???
  override def props: Map[String, String] = ???
  override def ilinks: List[Any] = ???

  /** optimize composiiotn of lists */
  override def +(other: BaseAstNode): BaseAstNode = other match {
    case ls: ListAstNode => ListAstNode(states ++ ls.states)
    case ps: BaseAstNode => ListAstNode(states ++ Seq(ps))
  }

  override def ifold(current: StrAstNode,
                     ctx: FoldingContext[_, _]): StrAstNode = {
    states.foldLeft(StrAstNode.EMPTY) { (a, b) =>
      val c = b.ifold(a, ctx)
      StrAstNode(a.s + c.s, a.props ++ c.props, a.ilinks ++ c.ilinks)
    }
  }
  override def toString = s"LSTATE (${states.mkString})"
  override def print(level: Int): String =
    ("--" * level) + "LState" + states.map(_.print(level + 1)).mkString
  override def printHtml(level: Int): String =
    "LState" + "<ul>" + states
      .map(x => "<li>" + x.printHtml(level + 1))
      .mkString + "</ul>"
}

/** an aggregation AST node - pattern is: prefix+midAST+suffix */
case class TriAstNode(prefix: String, mid: BaseAstNode, suffix: String)
    extends BaseAstNode {
  if (SpecParserSettings.debugAstNodes) cdebug << this.toString

  // nobody should ask for these - fold the parse result into a SState always
  override def s: String = ???
  override def props: Map[String, String] = ???
  override def ilinks: List[Any] = ???

  override def ifold(current: StrAstNode,
                     ctx: FoldingContext[_, _]): StrAstNode = {
    val c = mid.ifold(current, ctx)
    StrAstNode(prefix + c.s + suffix, c.props, c.ilinks)
  }
  override def toString = s"RSTATE ($prefix, $mid, $suffix)"
  override def print(level: Int): String =
    ("--" * level) + s"RSTATE ($prefix, $suffix)" + mid.print(level + 1)
  override def printHtml(level: Int): String =
    s"RSTATE ($prefix, $suffix)" + "<ul><li>" + mid.printHtml(level + 1) + "</ul>"
}

/** lazy AST node - value computed when they're folded.
  *
  * By default a lazy state will cause a non cacheable wiki
  */
case class LazyAstNode[T <: DSpec, U <: DUser](
  f: (StrAstNode, FoldingContext[T, U]) => StrAstNode
) extends BaseAstNode {
  var dirty = true

  if (SpecParserSettings.debugAstNodes) cdebug << this.toString

  // nobody should ask for these - fold the parse result into a SState always
  override def s: String = ???
  override def props: Map[String, String] = ???
  override def ilinks: List[Any] = ???

  override def ifold(current: StrAstNode,
                     ctx: FoldingContext[_, _]): StrAstNode = {
    if (dirty) ctx.cacheable = false
    f(current, ctx.asInstanceOf[FoldingContext[T, U]])
  }
  override def toString = s"LazySTATE ()"

  def cacheOk = { this.dirty = false; this }
}
