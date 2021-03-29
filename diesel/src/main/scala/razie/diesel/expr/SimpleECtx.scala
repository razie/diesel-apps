/*   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.expr

import razie.diesel.dom.RDOM.P
import razie.diesel.dom.RDomain
import razie.diesel.engine.nodes.{EVal, EWarning}
import razie.diesel.engine.{DomAst, DomEngECtx}
import razie.tconf.{DSpec, DTemplate}
import scala.collection.mutable
import scala.collection.mutable.{HashMap, ListBuffer}

/**
  * a context - LIST, use to see speed of list
  *
  * it's started with an initial set of values
  *
  * @param cur  - initial static values for this context
  * @param base - optional parent context
  * @param curNode
  */
class SimpleECtx(
  val cur: List[P] = Nil,
  val base: Option[ECtx] = None,
  val curNode: Option[DomAst] = None
) extends ECtx {

  protected var _domain: Option[RDomain] = None
  protected var _specs: List[DSpec] = Nil
  private var userId: Option[String] = None

  //payload is never static

  // overwritten values in this context
  var attrs: List[P] = Nil
  var _hostname: Option[String] = None

  // payload goes up always
  // only time this issue is when POST to diesel.rest and it will send it to the message as an attr PAYLOAD
  // that creates issues
//  if (cur.exists(_.name == Diesel.PAYLOAD))
//    root.engine.map(
//      _.warning(EWarning("It's non-intuitive to pass payload as argument! " + this.curNode.mkString.take(100))))

  def credentials: Option[String] = userId orElse base.flatMap(_.credentials)
  // once given, you cannot change credentials

  def withP(p: P) = {
    put(p)
    this
  }

  def withHostname(s: String) = {
    _hostname = Some(s)
    this
  }

  def withCredentials(s: Option[String]) = {
    if (userId.isEmpty) userId = s
    this
  }

  def listAttrs: List[P] = {
    val l = (attrs ++ cur ++ base.toList.flatMap(_.listAttrs)).distinct
    // distinct by name
    // copied distinct to do by name
    val b = new ListBuffer[P]()
    val seen = mutable.HashSet[String]()
    for (x <- l) {
      if (x.name.length > 0 && !seen(x.name)) {
        b += x
        seen += x.name
      }
    }
    b.toList
  }

  def domain: Option[RDomain] = _domain orElse base.flatMap(_.domain)

  def specs: List[DSpec] = _specs ::: base.toList.flatMap(_.specs)

  def hostname: Option[String] = _hostname orElse base.flatMap(_.hostname)

  def findTemplate(ea: String, direction: String = ""): Option[DTemplate] = {
    specs.flatMap(_.findSection(ea, direction).toList).headOption
  }

  /** find template with predicate */
  def findTemplate(p: DTemplate => Boolean): Option[DTemplate] = {
    specs.foldLeft[Option[DTemplate]](None)((a, s) =>
      // todo low stop searching when found
      a.orElse(s.findSection(p))
    )
  }

  /** check predicate on all values */
  def exists(f: scala.Function1[P, scala.Boolean]): scala.Boolean =
    cur.exists(f) || attrs.exists(f) || base.exists(_.existsNL(f))

  /** check predicate on all values, except locals */
  def existsNL(f: scala.Function1[P, scala.Boolean]): scala.Boolean =
    attrs.exists(f) || base.exists(_.existsNL(f))

  def remove(name: String): Option[P] = {
    attrs.find(a => a.name == name).map { p =>
      attrs = attrs.filter(_.name != name)
      // not removing from base
      p
    } orElse base.flatMap(_.remove(name))
  }

  override def clear: Unit = {
    attrs = Nil
    base.foreach(_.clear)
  }

  /** we delegate on empty values - empty is the same as missing then
    *
    * this is relevant - current message won't work otherwise, like ctx.echo(parm)
    * */
  def getp(name: String): Option[P] =
    None
        .orElse(attrs.find(a => a.name == name)) // was it overwritten?
        .orElse(cur.find(a => a.name == name && a.hasCurrentValue)) // or is it the static value?
        .orElse(base.flatMap(_.getp(name)))
        .orElse(getpFromDomain(name))

  /** see if we can source a value from the domain, i.e. static $val */
  protected def getpFromDomain(name: String): Option[P] = {
    domain.flatMap(_.moreElements.collectFirst {
      case v: EVal if v.p.name == name => v.p
    })
  }

  /** propagates by default up - see the Scope context which will not */
  override def put(p: P) {
    if (base.isDefined)
      base.get.put(p)
    //    else if(p.ttype == WTypes.UNDEFINED)
    //    remove(p)

    // overwrite locally if static - so overwriting arguments works.
    if (base.isEmpty || cur.exists(_.name == p.name))
      attrs = p :: attrs.filter(_.name != p.name)
  }

  /** propagates by default up - see the Scope context which will not */
  override def putAll(p: List[P]) {
    p.map(put)
//    if (base.isDefined) base.get.putAll(p)
//    else attrs = p ::: attrs.filter(x => !p.exists(_.name == x.name))
  }

  /** copy curr values - when this is replacing another type of context */
  def replacing(other: SimpleECtx) = {
    this.attrs = other.attrs
    this
  }

  override def root: DomEngECtx = base.map(_.root).getOrElse(this).asInstanceOf[DomEngECtx]

  override def toString = this.getClass.getSimpleName + ":cur==" +
      cur.mkString(",") + ":attrs==" + attrs.mkString(",") //+ base.map(_.toString).mkString
}
