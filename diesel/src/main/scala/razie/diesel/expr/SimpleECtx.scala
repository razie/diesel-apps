/*   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.expr

import razie.diesel.dom.RDOM.P
import razie.diesel.dom.RDomain
import razie.diesel.engine.nodes.EVal
import razie.diesel.engine.{DomAst, DomEngECtx}
import razie.tconf.{DSpec, DTemplate}

/** a context - LIST, use to see speed of list
  *
  * @param cur
  * @param base
  * @param curNode
  */
class SimpleECtx(val cur: List[P] = Nil, val base: Option[ECtx] = None, val curNode:Option[DomAst] = None) extends ECtx {
  protected var _domain: Option[RDomain] = None
  protected var _specs: List[DSpec] = Nil
  private var userId:Option[String] = None

  var attrs: List[P] = Nil
  var _hostname:Option[String] = None

  def credentials: Option[String] = userId orElse base.flatMap(_.credentials)
  // once given, you cannot change credentials

  def withP(p:P) = {
    put(p)
    this
  }

  def withHostname(s: String) = {
    _hostname = Some(s)
    this
  }

  def withCredentials(s: Option[String]) = {
    if(userId.isEmpty) userId = s
    this
  }

  def listAttrs: List[P] = cur ++ attrs ++ base.toList.flatMap(_.listAttrs)

  def domain: Option[RDomain] = base.map(_.domain) getOrElse _domain
  def specs: List[DSpec] = base.map(_.specs) getOrElse _specs

  def hostname: Option[String] = _hostname orElse base.flatMap(_.hostname)

  def findTemplate (ea:String, direction:String="") : Option[DTemplate] = {
    specs.flatMap(_.findSection(ea, direction).toList).headOption
  }

  /** find template with predicate */
  def findTemplate (p : DTemplate => Boolean) : Option[DTemplate] = {
    specs.foldLeft[Option[DTemplate]](None)((a,s) =>
      // todo low stop searching when found
      a.orElse(s.findSection(p))
    )
  }

  def exists(f: scala.Function1[P, scala.Boolean]): scala.Boolean =
    cur.exists(f) || attrs.exists(f) || base.exists(_.exists(f))

  def remove (name: String): Option[P] = {
    attrs.find(a=> a.name == name).map { p =>
      attrs = attrs.filter(_.name != name)
      // not removing from base
      p
    } orElse base.flatMap(_.remove(name))
  }

  override def clear = {
    attrs = Nil
    base.map(_.clear)
  }

  /** we delegate on empty values - empty is the same as missing then
    *
    * this is relevant - current message won't work otherwise, like ctx.echo(parm)
    * */
  def getp(name: String): Option[P] =
    None
      .orElse(cur.find(a=> a.name == name && a.hasCurrentValue)) // or is it the static value?
      .orElse(attrs.find(a=> a.name == name)) // was it overwritten?
      .orElse(base.flatMap(_.getp(name)))
      .orElse(getpFromDomain(name))

  private def getpFromDomain(name:String) : Option[P] = {
    domain.flatMap(_.moreElements.collectFirst{
      case v:EVal if v.p.name == name => v.p
    })
  }

  /** propagates by default up - see the Scope context which will not */
  def put(p: P): Unit =
    if(base.isDefined)
      base.get.put(p)
    //    else if(p.ttype == WTypes.UNDEFINED)
    //    remove(p)
    else
      attrs = p :: attrs.filter(_.name != p.name)

  /** propagates by default up - see the Scope context which will not */
  def putAll(p: List[P]): Unit =
    if(base.isDefined) base.get.putAll(p)
    else attrs = p ::: attrs.filter(x => !p.exists(_.name == x.name))
    // p.map(put)
    // this old ver would not remove null/UNDEF

  def root: DomEngECtx = base.map(_.root).getOrElse(this).asInstanceOf[DomEngECtx]

  override def toString = this.getClass.getSimpleName + ":cur==" +
    cur.mkString(",") + ":attrs==" + attrs.mkString(",") //+ base.map(_.toString).mkString
}


