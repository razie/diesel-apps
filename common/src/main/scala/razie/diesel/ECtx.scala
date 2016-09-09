package razie.diesel

import razie.diesel.RDOM.P
import razie.wiki.model.{WikiSection, WikiEntry}

/*
 * a map like context of attribute values.
 *
 * contexts are hierarchical, with inheritance
 *
 * also, they have an optional domain
 */
trait ECtx {
  def root: ECtx
  def base: Option[ECtx]
  def hostname:String

  def domain: Option[RDomain]
  def specs: List[WikiEntry]
  def domain_= (x:Option[RDomain])
  def specs_= (x : List[WikiEntry])

  def findTemplate (ea:String) : Option[WikiSection]

  def exists(f: scala.Function1[P, scala.Boolean]): scala.Boolean

  def apply  (name: String): String
  def get    (name: String): Option[String]
  def put    (p: P): Unit
  def putAll (p: List[P]): Unit
}

// a context - LIST, use to see speed of list
class SimpleECtx(val cur: List[P] = Nil, val base: Option[ECtx] = None) extends ECtx {
  var attrs: List[P] = Nil
  var _domain: Option[RDomain] = None
  var _specs: List[WikiEntry] = Nil
  var hostname:String = ""

  def domain: Option[RDomain] = base.map(_.domain) getOrElse _domain
  def specs: List[WikiEntry] = base.map(_.specs) getOrElse _specs

  def domain_= (x:Option[RDomain])  = base.map(_.domain = x) getOrElse (_domain = x)
  def specs_= (x : List[WikiEntry]) = base.map(_.specs = x) getOrElse (_specs = x)

  def withSpecs(s: List[WikiEntry]) = {
    specs = s ::: specs
    this
  }

  def withDomain(r: RDomain) = {
    domain = Some(r)
    this
  }

  def findTemplate (ea:String) : Option[WikiSection] = {
    specs.flatMap(_.templateSections.filter(_.name == ea)).headOption
  }

  def apply(name: String): String = get(name).mkString

  def exists(f: scala.Function1[P, scala.Boolean]): scala.Boolean =
    cur.exists(f) || attrs.exists(f) || base.exists(_.exists(f))

  def get(name: String): Option[String] =
    cur.find(_.name == name).orElse(
      attrs.find(_.name == name)).map(_.dflt).orElse(
      base.flatMap(_.get(name)))

  def put(p: P): Unit = attrs = p :: attrs.filter(_.name != p.name)

  def putAll(p: List[P]): Unit = attrs = p ::: attrs.filter(x => !p.exists(_.name == x.name))

  def root: ECtx = base.map(_.root).getOrElse(this)
}

/** static context will delegate updates to parent */
class StaticECtx(cur: List[P] = Nil, base: Option[ECtx] = None) extends SimpleECtx(cur, base) {
  //todo should I throw up if no base?
  override def put(p: P): Unit = base.map(_.put(p))

  override def putAll(p: List[P]): Unit = base.map(_.putAll(p))
}

/** specific root context for an enging instance */
class DomEngECtx(cur: List[P] = Nil, base: Option[ECtx] = None) extends SimpleECtx(cur, base) {
  var overwritten: Option[ECtx] = None
  var persisted: Boolean = false

  override def apply(name: String): String = overwritten.map(_.apply(name)).getOrElse(super.apply(name))

  override def get(name: String): Option[String] = overwritten.map(_.get(name)).getOrElse(super.get(name))

  override def put(p: P): Unit = overwritten.map(_.put(p)).getOrElse(super.put(p))

  override def putAll(p: List[P]): Unit = overwritten.map(_.putAll(p)).getOrElse(super.putAll(p))

  override def root: ECtx = overwritten.map(_.root).getOrElse(super.root)

  override def exists(f: scala.Function1[P, scala.Boolean]): scala.Boolean =
    overwritten.map(_.exists(f)).getOrElse(super.exists(f))

  def overwrite(ctx: ECtx) =
    if (this != ctx)
      overwritten = Some(ctx)
}

/** context for an internal scope - parent is scope or Eng
  *
  * todo when saving a context, do I save children too?
  *
  * todo when loading context, how do I reover active scope contexts
  */
class ScopeECtx(cur: List[P] = Nil, base: Option[ECtx] = None) extends SimpleECtx(cur, base) {
}

object ECtx {
  /** empty context */
  val empty = new StaticECtx()
}

