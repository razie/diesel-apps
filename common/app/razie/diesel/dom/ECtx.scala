package razie.diesel.dom

import razie.diesel.dom.RDOM.P
import mod.diesel.model.{DomAst}
import org.bson.types.ObjectId

/*
 * a map like context of attribute values.
 *
 * contexts are hierarchical, with inheritance
 *
 * They also capture a spec environment: a list of specs (could be drafts or a specific version)
 *
 * Also, they have an optional domain
 *
 * keep a ref to the original specs, to get more details
 *
 * So, for the duration of this context, the configuration is the right version
 */
trait ECtx {
  /** root domain - it normally is an instance of DomEngineCtx and you can get more details from it */
  def root: ECtx
  /** in a hierarchy, this is my failback */
  def base: Option[ECtx]
  def hostname:Option[String]

  def credentials: Option[String] // original credentials
  /** the domain - normally this is only set in the root EngineCtx */
  def domain: Option[RDomain]
  /** the specs for this engine - normally this is only set in the root EngineCtx */
  def specs: List[DSpec]

  def findTemplate (ea:String) : Option[DTemplate]

  def exists(f: scala.Function1[P, scala.Boolean]): scala.Boolean

  def apply  (name: String): String = get(name).mkString
  def getp   (name: String): Option[P] // overwrite this one - leave the get
  def get    (name: String): Option[String] = getp(name).map(_.dflt)
  def put    (p: P): Unit
  def putAll (p: List[P]): Unit

  def curNode : Option[DomAst]

  /** see if this is a qualified name in a structure */
  def sourceStruc (name:String, root:Option[Map[String,Any]] = None) : Option[P] = {
    val x:Option[String] = if (name contains ".") {
      val R = """([^.]+)\.(.*)""".r
      val R(n,rest) = name
      root.flatMap(_.get(n)).orElse {
        get(n).map ( razie.js.parse )
      }.collect {
        case x:Map[String, _] => sourceStruc(rest, Some(x)).map(_.dflt)
      }.flatten
//    } else root.flatMap(_.get(name)).map(xx=> Map(name -> razie.js.anytojson(xx)))
    } else root.flatMap(_.get(name)).map(razie.js.anytojsons)
    x.map(x=>P(name, x))
  }
}

// a context - LIST, use to see speed of list
class SimpleECtx(val cur: List[P] = Nil, val base: Option[ECtx] = None, val curNode:Option[DomAst]) extends ECtx {
  var attrs: List[P] = Nil
  var _domain: Option[RDomain] = None
  var _specs: List[DSpec] = Nil
  var _hostname:Option[String] = None
  private var userId:Option[String] = None

  def credentials: Option[String] = userId orElse base.flatMap(_.credentials)
  // once given, you cannot change credentials
  def withCredentials(s: Option[String]) = {
    if(userId.isEmpty) userId = s
    this
  }

  def domain: Option[RDomain] = base.map(_.domain) getOrElse _domain
  def specs: List[DSpec] = base.map(_.specs) getOrElse _specs

  def hostname: Option[String] = _hostname orElse base.flatMap(_.hostname)

//  def domain_= (x:Option[RDomain])  = base.map(_.domain = x) getOrElse (_domain = x)
//  def specs_= (x : List[DSpec]) = base.map(_.specs = x) getOrElse (_specs = x)

  def findTemplate (ea:String) : Option[DTemplate] = {
    specs.flatMap(_.findTemplate(ea).toList).headOption
  }


  def exists(f: scala.Function1[P, scala.Boolean]): scala.Boolean =
    cur.exists(f) || attrs.exists(f) || base.exists(_.exists(f))

  /** we delegate on empty values - empty is the same as missing then
    *
    * this is relevant - current message won't work otherwise, like ctx.echo(parm)
    * */
  def getp(name: String): Option[P] =
    cur.find(a=> a.name == name && a.dflt != "").orElse(
      attrs.find(a=> a.name == name)).orElse(
      base.flatMap(_.getp(name))).orElse(
      sourceStruc(name)
    )

  /** propagates by default up - see the Scope context which will not */
  def put(p: P): Unit =
    if(base.isDefined) base.get.put(p)
    else  attrs = p :: attrs.filter(_.name != p.name)

  /** propagates by default up - see the Scope context which will not */
  def putAll(p: List[P]): Unit =
    if(base.isDefined) base.get.putAll(p)
    else attrs = p ::: attrs.filter(x => !p.exists(_.name == x.name))

  def root: ECtx = base.map(_.root).getOrElse(this)

  override def toString =
    cur.mkString(",") + attrs.mkString(",") + base.map(_.toString).mkString
}

/** static context will delegate updates to parent */
class StaticECtx(cur: List[P] = Nil, base: Option[ECtx] = None, curNode:Option[DomAst]=None) extends SimpleECtx(cur, base, curNode) {
  //todo should I throw up if no base?
  override def put(p: P): Unit = base.map(_.put(p))

  override def putAll(p: List[P]): Unit = base.map(_.putAll(p))
}

/** context for an internal scope - parent is scope or Eng
  *
  * todo when saving a context, do I save children too?
  *
  * todo when loading context, how do I reover active scope contexts
  */
class ScopeECtx(cur: List[P] = Nil, base: Option[ECtx] = None, curNode:Option[DomAst]=None) extends SimpleECtx(cur, base, curNode) {
//  override def put(p: P): Unit =
//    attrs = p :: attrs.filter(_.name != p.name)
//
//  override def putAll(p: List[P]): Unit =
//    attrs = p ::: attrs.filter(x => !p.exists(_.name == x.name))
}

object ECtx {
  /** empty context */
  val empty = new StaticECtx()
}
