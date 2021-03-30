/*  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import api.dwix
import org.bson.types.ObjectId
import razie.diesel.dom.RDOM.{P, ParmSource}
import razie.diesel.dom.RDomain
import razie.diesel.expr.{ECtx, SimpleECtx}
import razie.diesel.model.DieselMsg
import razie.hosting.Website
import razie.tconf.{DSpec, DUsers}
import razie.wiki.Config


/** specific root context for an engine instance
  *
  * it can keep unique IDs and such, to help with tests ran continuously
  */
class DomEngECtx(val settings: DomEngineSettings, cur: List[P] = Nil, base: Option[ECtx] = None)
    extends SimpleECtx(cur, base, None) {
  var overwritten: Option[ECtx] = None
  var persisted: Boolean = false
  var engine: Option[DomEngine] = None

  override def name = "DomEngCtx"

  /** NOT for moving from engine to engine */
  def withEngine(e: DomEngine) = {
    this.engine = Some(e);
    this
  }

  def withSpecs(s: List[DSpec]) = {
    _specs = s ::: _specs
    this
  }

  def withDomain(r: RDomain) = {
    _domain = Some(r)
    this
  }

  override def apply(name: String): String =
    overwritten
        .map(_.apply(name))
        // todo not call ps to parse random payloads
        // have a separate expression for body access or are we adding each in ctx ?
//        .orElse(ps(name).map(_.currentStringValue))
        .orElse(pu(name).map(_.currentStringValue))
        .getOrElse(super.apply(name))

  override def getp(name: String): Option[P] =
    if(name.length > 0)
      overwritten
          .flatMap(_.getp(name))
          // todo not call ps to parse random payloads
          // have a separate expression for body access or are we adding each in ctx ?
//          .orElse(ps(name))
          .orElse(pu(name))
          .orElse(super.getp(name))
    else None

  override def put(p: P) {
    overwritten.map(_.put(p)).getOrElse(super.put(p))
  }

  override def putAll(p: List[P]) {
    overwritten.map(_.putAll(p)).getOrElse(super.putAll(p))
  }

  override def root = overwritten.map(_.root).getOrElse(super.root)


  override def exists(f: scala.Function1[P, scala.Boolean]): scala.Boolean =
    overwritten
        .map(_.exists(f))
        // todo remove use of postedContent - what the heck, see ps() as well
//        .orElse(settings.postedContent.map(_.exists(f)))
        .getOrElse(super.exists(f))


  /** set some of the unique values to help rerun tests */
  def setu (p:P) : Boolean = p.name match {
    case "diesel.settings.mockQuery" => true
    case _ => false
  }

  // figure out the environment for this user
  // todo once the engine starts, store the start value in settings - it can't change and we should store it,m if it's not overwritten...
  def dieselEnv(ctx:ECtx) = {
    val settings = ctx.root.engine.map(_.settings)
    val au = settings
        .flatMap(_.userId)
        .map(new ObjectId(_))
        .flatMap(DUsers.impl.findUserById)

    val ret = dwix.dieselEnvFor(settings.flatMap(_.realm).mkString, au)
    ret
  }

  // user id if any
  def dieselUser(ctx: ECtx) = {
    val au = ctx.root.engine.map(_.settings).flatMap(_.userId)
    au.mkString
  }

  // user id if any
  def dieselUsername(ctx: ECtx) = {
    val au = ctx.root.engine.map(_.settings).flatMap(_.userId)
    val x = au.flatMap(id => DUsers.impl.findUserById(new ObjectId(id))).map(_.userName)
    x.mkString
  }

  /** source from settings - only if there's some value... otherwise base won't cascade */
  private def ps(name: String): Option[P] =
    settings.postedContent.filter(_.body.length > 0).flatMap(_.getp(name))

  /** used for instance when persisting a context - will overwrite the default */
  def overwrite(ctx: ECtx): Unit =
    if (this != ctx)
      overwritten = Some(ctx)

  /** reset this engine's values */
  override def clear: Unit = {
    this.attrs = Nil
    this.overwritten.foreach(_.clear)
    this.base.foreach(_.clear)
  }

  override def toString = this.getClass.getSimpleName + ":cur==" +
      cur.mkString(",") + ":attrs==" + attrs.mkString(",") //+ base.map(_.toString).mkString
}

/** source for parms starting with "diesel" */
class DieselParmSource (ctx:DomEngECtx) extends ParmSource {
  def name = "diesel"

  def remove(name: String): Option[P] = ???

  def getp(name: String): Option[P] = name match {

    case "env" =>
      // first overrides, then settings and lastly current envList setting
      None
          .orElse(ctx.settings.env.map(P("diesel.env", _)))
          .orElse(Some(P("diesel.env", ctx.dieselEnv(ctx))))

    case "user" => Some(P("diesel.user", ctx.dieselUser(ctx)))
    case "username" => Some(P("diesel.username", ctx.dieselUsername(ctx)))
    case "isLocalhost" => Some(P.fromTypedValue("diesel.isLocalhost", razie.wiki.Services.config.isLocalhost))

    case "realm" => {
      next("diesel.realm", Map(
        "props" -> (n => Right(new DieselRealmParmSource(ctx)))
      ))
    }

    case "server" => Some(P.fromSmartTypedValue("diesel.server", Map(
      "node" -> Config.node,
      "host" -> java.net.InetAddress.getLocalHost.getCanonicalHostName,
      "hostName" -> java.net.InetAddress.getLocalHost.getHostName,
      "ip" -> java.net.InetAddress.getLocalHost.getHostAddress
    )))

    case _ => None
  }

  def next(name: String, values: Map[String, String => Either[P, ParmSource]]) =
    Some(P.fromTypedValue(name, new NextDParmSource(ctx, name, values)))

  def put(p: P): Unit = throw new DieselException("Can't overwrite values in this context!")

  def listAttrs: List[P] = Nil

}

/** source for parms static at realm level */
class DieselCtxParmSource(val name: String, ctx: ECtx, origCtx: ECtx) extends ParmSource {

  def remove(name: String): Option[P] = ctx.remove(name)

  def getp(name: String): Option[P] = ctx.getp(name)

  def put(p: P): Unit = {
    if ("dieselScope" == name) {
      // remove any overload from all contexts until the scope
      origCtx.allToScope.foreach(
        _.remove(p.name)
      )

      // scope vars are set in the closest enclosing ScopeECtx or EngCtx
      // the idea is to bypass the enclosing RuleScopeECtx
    }

    ctx.put(p)
  }

  def listAttrs: List[P] = ctx.listAttrs
}

/** source for parms static at realm level */
class DieselRealmParmSource(ctx: DomEngECtx) extends ParmSource {
  val realm = ctx.root.settings.realm.mkString

  def parms = {
    Website.getRealmProps(realm)
  }

  def name = "diesel.realm.props"

  def remove(name: String): Option[P] = ???

  def getp(name: String): Option[P] = name match {
    case _ => {
      val p = parms
      p.get(name)
    }
  }

  def put(p: P): Unit = {
    Website.putRealmProps(realm, p.name, p.calculatedP(ctx))
    Website.forRealm(realm).map(_.put(p.name, p.calculatedValue(ctx)))
  }

  def listAttrs: List[P] = {
    val p = parms
    p.values.toList
  }

}

/** todo hierarchical source for objects? */
class NextParmSource(ctx: DomEngECtx, pname: String, value: P) extends ParmSource {
  def name = pname

  def remove(name: String): Option[P] = ???

  def getp(name: String): Option[P] = name match {
    case pname => Some(value)
    case _ => None
  }

  def put(p: P): Unit = ???

  def listAttrs: List[P] = ???
}

/** todo hierarchical source for objects? */
class NextDParmSource(ctx: ECtx, pname: String, values: Map[String, String => Either[P, ParmSource]]) extends
    ParmSource {
  def name = pname

  def getp(name: String): Option[P] = values.get(name).flatMap(_.apply(name) match {
    case Left(p) => Some(p)
    case Right(ps) => Some(P.fromSmartTypedValue(pname + "." + name, ps))
  })

  def remove(name: String): Option[P] = ???

  def put(p: P): Unit = ???

  def listAttrs: List[P] = ???
}

