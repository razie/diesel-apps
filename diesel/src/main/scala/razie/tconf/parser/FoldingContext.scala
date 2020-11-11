/**
  *   ____    __    ____  ____  ____,,___     ____  __  __  ____
  *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 **/
package razie.tconf.parser

import razie.tconf.{DSpec, DUser}

/** folding context base class - contains the page being parsed and for which user
  *
  * we use these to defer execution of different elements during parsing. These contain the
  * context in which the current parse works.
  * */
abstract class FoldingContext[+T <: DSpec, +U <: DUser] {

  /** current spec being parsed */
  def we: Option[T]

  /** user, if any, for which this is parsed */
  def au: Option[U]

  /** why is this being parsed? template, view or pre-processed */
  def target: String

  /** evaluate expressions in the folded content
    * kind can be "$" or "$$"
    *
    * Expressions can be simple parm access like $name OR api.wix
    */
  def eval(kind: String, expr: String): String

  def wpath = we.map(_.specRef.wpath)

  /** is this cacheable as parsed or not? */
  def cacheable: Boolean = we.map(_.cacheable).getOrElse(false)
  def cacheable_=(v: Boolean) = we.foreach(_.cacheable = v)
}

/** folding context using a map for the properties available to evaluate expressions */
class JMapFoldingContext[T <: DSpec, U <: DUser](val we: Option[T],
                                                 val au: Option[U],
                                                 val target: String = T_VIEW,
                                                 val ctx: Map[String, Any] =
                                                   Map.empty)
    extends FoldingContext[T, U] {

  /** kind can be "$" or "$$" */
  def eval(kind: String, expr: String): String =
    (if (kind == "$$" && target == T_TEMPLATE || kind == "$")
       ex(ctx, expr.split("\\."))
     else None) getOrElse s"`{{$kind$expr}}`"

  /** resolve a name a.b.c in a given context, for simple expressions */
  private def ex(m: Map[String, Any], terms: Array[String]): Option[String] =
    if (terms.size > 0 && m.contains(terms(0))) m(terms(0)) match {
      case m: collection.Map[_, _] if terms.size > 1 =>
        ex(m.asInstanceOf[Map[String, Any]], terms.drop(1))
      case s: String  => Some(s)
      case l: List[_] => Some(l.mkString)
      case h @ _      => Some(h.toString)
    } else None
}


