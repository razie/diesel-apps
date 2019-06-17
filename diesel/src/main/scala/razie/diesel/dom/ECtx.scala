package razie.diesel.dom

import razie.{cdebug, clog}
import razie.diesel.dom.RDOM.P
import razie.diesel.engine.DomEngECtx
import razie.diesel.ext.EVal
import razie.tconf.{DSpec, DTemplate}

import scala.util.Try

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
  def get    (name: String): Option[String] = getp(name).map(_.dflt)
  def put    (p: P): Unit
  def putAll (p: List[P]): Unit
  def clear  : Unit
  def listAttrs: List[P]

  def curNode : Option[DomAst]

  /** see if this is a qualified name in a structure */
  def sourceStruc (name:String, root:Option[Map[String,Any]] = None) : Option[P] = {
    val x:Option[_] = if (name contains ".") {
      val R = """([^.]+)\.(.*)""".r
      val R(n,rest) = name
      root.flatMap(_.get(n)).orElse {
        Try {
          val p = getp(n).filter(_.dflt.length > 0)
          val m =
            if(p.flatMap(_.value).exists(_.isInstanceOf[Map[_,_]]))
              p.flatMap(_.value).map(_.asInstanceOf[Map[_,_]])
          else {
              // if json try to parse it
            p.map(_.dflt).map{v=>
              if(v.trim.startsWith("{"))
                razie.js.parse(v)
              else Map.empty
            }
            }
          m
        }.recover {
          Map.empty
        }.get
      }.collect {
        case x:Map[_, _] => sourceStruc(rest, Some(x.asInstanceOf[Map[String, Any]]))
      }.flatten
    } else {
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


/** a context - LIST, use to see speed of list */
class SimpleECtx(val cur: List[P] = Nil, val base: Option[ECtx] = None, val curNode:Option[DomAst] = None) extends ECtx {
  var attrs: List[P] = Nil
  var _domain: Option[RDomain] = None
  var _specs: List[DSpec] = Nil
  var _hostname:Option[String] = None
  private var userId:Option[String] = None

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

//  def domain_= (x:Option[RDomain])  = base.map(_.domain = x) getOrElse (_domain = x)
//  def specs_= (x : List[DSpec]) = base.map(_.specs = x) getOrElse (_specs = x)

  def findTemplate (ea:String, direction:String="") : Option[DTemplate] = {
    specs.flatMap(_.findSection(ea, direction).toList).headOption
  }

  /** find template with predicate */
  def findTemplate (p : DTemplate => Boolean) : Option[DTemplate] = {
    specs.foldLeft[Option[DTemplate]](None)((a,s) =>
      // stop searching when found
      a.orElse(s.findSection(p))
    )
  }

  def exists(f: scala.Function1[P, scala.Boolean]): scala.Boolean =
    cur.exists(f) || attrs.exists(f) || base.exists(_.exists(f))

  def remove (name: String): Option[P] = {
    attrs.find(a=> a.name == name).map { p =>
      attrs = attrs.filter(_.name != name)
//      base.flatMap(_.remove(name)) // remove anyways from base, in case it was defined
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
    cur.find(a=> a.name == name && a.dflt != "")
      .orElse(attrs.find(a=> a.name == name))
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
//      remove(p)
    else
      attrs = p :: attrs.filter(_.name != p.name)

  /** propagates by default up - see the Scope context which will not */
  def putAll(p: List[P]): Unit =
    if(base.isDefined) base.get.putAll(p)
    else attrs = p ::: attrs.filter(x => !p.exists(_.name == x.name))
//    p.map(put)
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
  private def check (p:P) = {
//    if(cur.exists(_.name == p.name)) {
//      razie.Log.warn("WARNING_OVERWRITE at put - you may be overwriting side-effects: "+p.name)
//    }
  }

  //todo should I throw up if no base?
  override def put(p: P): Unit = {
    check(p)
    base.map(_.put(p))
  }

  override def putAll(p: List[P]): Unit = {
    p.map(check)
    base.map(_.putAll(p))
  }

  override def remove (name: String): Option[P] = base.flatMap(_.remove(name))
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

