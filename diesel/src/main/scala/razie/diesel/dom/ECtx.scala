/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.dom

import razie.diesel.dom.RDOM.P
import razie.diesel.engine.{DomAst, DomEngECtx}
import razie.diesel.ext.{EVal, EWarning}
import razie.tconf.{DSpec, DTemplate}
import scala.util.Try

/**
 * A map-like context of attribute values, used by the Diesel engine.
 *
 * These contexts are hierarchical, with inheritance and overwriting. Each engine has a root context. There are also
 * scope contexts (which don't allow propagation of values) etc
 *
 * They also capture a spec environment: a list of specs (could be drafts or a specific version)
 *
 * Also, they have an optional domain - this is used to source values and functions and other objects for expressions.
 * Normally, the domain is set by the engine at the root context level.
 *
 * todo keep a ref to the original specs, to get more details, so for the duration of this context, the configuration is the right version
 */
trait ECtx {

  /** root domain - it normally is an instance of DomEngineCtx and you can get more details from it */
  def root: DomEngECtx
  /** in a hierarchy, this is my failback */
  def base: Option[ECtx]
  def hostname:Option[String]

  def credentials: Option[String] // original credentials
  /** the domain - normally this is only set in the root EngineCtx */
  def domain: Option[RDomain]
  /** the specs for this engine - normally this is only set in the root EngineCtx */
  def specs: List[DSpec]

  /** find the template corresponding to the ea and direction (direction is optional
    *
    * @param ea entity.action
    * @param direction "request" vs "response"
    * @return
    */
  def findTemplate (ea:String, direction:String="") : Option[DTemplate]

  /** find template with predicate */
  def findTemplate (p : DTemplate => Boolean) : Option[DTemplate]

  def exists(f: scala.Function1[P, scala.Boolean]): scala.Boolean

  def remove (name: String): Option[P]
  def apply  (name: String): String = get(name).mkString
  def getp   (name: String): Option[P] // overwrite this one - leave the get
  def get    (name: String): Option[String] = getp(name).map(_.currentStringValue)
  def put    (p: P): Unit
  def putAll (p: List[P]): Unit
  def clear  : Unit
  def listAttrs: List[P]

  def getRequired   (name: String): String = getRequiredp(name).currentStringValue
  def getRequiredp  (name: String): P = {
    val p = getp(name)
    if(! p.isDefined) throw new IllegalArgumentException(s"'$name' not found!")
    p.get
  }

  def curNode : Option[DomAst]

  /** see if this is a qualified name in a structure
    * @deprecated - use AExprIdent instead
    */
  def sourceStruc (name:String, root:Option[Map[String,Any]] = None) : Option[P] = {
    val x:Option[_] = if (name contains ".") {
      try {
        val R = """([^.]+)\.(.*)""".r
        val R(n, rest) = name
        root.flatMap(_.get(n)).orElse {
          Try {
            val p = getp(n).filter(_.hasCurrentValue)
            val m =
              if (p.flatMap(_.value.map(_.value)).exists(_.isInstanceOf[Map[_, _]]))
                p.flatMap(_.value.map(_.value)).map(_.asInstanceOf[Map[_, _]])
              else {
                // if json try to parse it
                p.map(_.currentStringValue).map { v =>
                  if (v.trim.startsWith("{"))
                    razie.js.parse(v)
                  else Map.empty
                }
              }
            m
          }.recover {
            Map.empty
          }.get
        }.collect {
          case x: Map[_, _] => sourceStruc(rest, Some(x.asInstanceOf[Map[String, Any]]))
        }.flatten
      }
      catch {
        case t: Throwable => throw new IllegalArgumentException(s"Can't sourceStruc parmname: ${name.take(100)}", t)
      }
    }
      else
      {
        val xxx = root.flatMap(_.get(name))
        val s = xxx.map(razie.js.anytojsons)
        s.map { s =>
          P(name, s, WTypes.typeOf(xxx.get)).withValue(xxx.get, WTypes.typeOf(xxx.get))
        }
      }

    if(x.exists(_.isInstanceOf[P]))
      Some(x.get.asInstanceOf[P])
    else
    // todo typed
      x.map(x=>P(name, x.toString))
  }
}


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
      .orElse(sourceStruc(name))
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

/** static context will delegate updates to parent - good as temporary override when evaluating a message */
class StaticECtx(cur: List[P] = Nil, base: Option[ECtx] = None, curNode:Option[DomAst]=None) extends SimpleECtx(cur, base, curNode) {

  // check for overwriting values
  // todo if i'm in a message, this will always be the case - why am I alarming?
  // todo and who can see it? it can't be in the logs...
  def check (p:P):Option[EWarning] = {
    if(cur.exists(_.name == p.name)) {
      Some(EWarning("WARNING_OVERWRITE at put - you may be overwriting ide-effects: "+p.name))
    } else None
  }

  //todo should I throw up if no base?
  override def put(p: P): Unit = {
    check(p)
    // propagate to base, so it lives
    base.map(_.put(p))
  }

  override def putAll(p: List[P]): Unit = {
    p.map(check)
    // propagate to base, so it lives
    base.map(_.putAll(p))
  }

  override def remove (name: String): Option[P] = {
    base.flatMap(_.remove(name))
  }

  override def clear = {
    base.map(_.clear)
  }

  override def toString = this.getClass.getSimpleName + ":" + cur.mkString //+ "\n base: " +base.toString
}

/** static context will delegate updates to parent - good as temporary override when evaluating a message */
class StaticECtxOverride(cur: List[P] = Nil, base: Option[ECtx] = None, curNode:Option[DomAst]=None) extends SimpleECtx(cur, base, curNode) {

  // copy overriding values from base - this is important when values are overwritten and the ctx is re-created
  attrs = base.toList.flatMap(_.asInstanceOf[SimpleECtx].attrs.filter{p=> cur.exists{c=> c.name == p.name}}) // attrs must have been calculated...

  // check for overwriting values
  // todo if i'm in a message, this will always be the case - why am I alarming?
  // todo and who can see it? it can't be in the logs...
  private def check (p:P) = {
//    if(cur.exists(_.name == p.name)) {
//      razie.Log.warn("WARNING_OVERWRITE at put - you may be overwriting side-effects: "+p.name)
//    }
  }

  //todo should I throw up if no base?
  override def put(p: P): Unit = {
    check(p)
    // propagate to base, so it lives
    base.map(_.put(p))
    // also overwrite here...
    attrs = p :: attrs.filter(_.name != p.name)
  }

  override def putAll(p: List[P]): Unit = {
    p.map(check)
    // propagate to base, so it lives
    base.map(_.putAll(p))
    // also overwrite here...
    attrs = p ::: attrs.filter(x => !p.exists(_.name == x.name))
  }

  override def remove (name: String): Option[P] = {
    attrs = attrs.filter(x => name == x.name)
    base.flatMap(_.remove(name))
  }

  override def clear = {
    base.map(_.clear)
  }

  override def toString = this.getClass.getSimpleName + ":" + cur.mkString //+ "\n base: " +base.toString
}

/** context for an internal scope - parent is scope or Eng
  *
  * todo when saving a context, do I save children too?
  *
  * todo when loading context, how do I reover active scope contexts
  */
class ScopeECtx(cur: List[P] = Nil, base: Option[ECtx] = None, curNode:Option[DomAst]=None) extends SimpleECtx(cur, base, curNode) {
  override def put(p: P): Unit =
    attrs = p :: attrs.filter(_.name != p.name)

  override def putAll(p: List[P]): Unit =
    attrs = p ::: attrs.filter(x => !p.exists(_.name == x.name))

  override def clear = {
    attrs = Nil
    // don't cascade to base
  }

  override def toString = this.getClass.getSimpleName + ":" + cur.mkString// + "\n base: " +base.toString
}

object ECtx {
  /** empty context */
  val empty = new StaticECtx()

  def apply (attrs:List[P]) = new StaticECtx(attrs)
}

