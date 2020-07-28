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


/** specific root context for an engine instance
  *
  * it can keep unique IDs and such, to help with tests ran continuously
  */
class DomEngECtx(val settings:DomEngineSettings, cur: List[P] = Nil, base: Option[ECtx] = None) extends SimpleECtx(cur, base, None) {
  var overwritten: Option[ECtx] = None
  var persisted: Boolean = false
  var engine : Option[DomEngine] = None

  /** NOT for moving from engine to engine */
  def withEngine(e:DomEngine) = { this.engine = Some(e); this}

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
        .orElse(ps(name).map(_.currentStringValue))
        .orElse(pu(name).map(_.currentStringValue))
        .getOrElse(super.apply(name))

  override def getp(name: String): Option[P] =
    if(name.length > 0)
      overwritten
          .flatMap(_.getp(name))
          .orElse(ps(name))
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
        .orElse(settings.postedContent.map(_.exists(f)))
        .getOrElse(super.exists(f))

  // uniques
  val uid = P("DIESEL_UID", new ObjectId().toString)
  val millis = P("DIESEL_MILLIS", System.currentTimeMillis().toString)

  /** source some of the unique values to help rerun tests */
  def pu (s:String) : Option[P] = s match {
    case "DIESEL_UID" => Some(uid)
    case "DIESEL_MILLIS" => Some(millis)
    case "DIESEL_CURMILLIS" => Some(P("DIESEL_CURMILLIS", System.currentTimeMillis().toString))

    // allow setting this
    case "diesel.env" =>
      // first overrides, then settings and lastly current envList setting
      super
          .getp(s)
          .orElse(settings.env.map(P("diesel.env",_)))
          .orElse(Some(P("diesel.env", dieselEnv(this))))

    case "diesel.user" => Some(P("diesel.user", dieselUser(this)))
    case "diesel.isLocalhost" => Some(P.fromTypedValue("diesel.isLocalhost", razie.wiki.Services.config.isLocalhost))
    case "diesel.db.newId" => Some(P("diesel.db.newId", new ObjectId().toString))
    case DieselMsg.ENGINE.DIESEL_ENG_DESC => Some(P(DieselMsg.ENGINE.DIESEL_ENG_DESC, engine.map(_.description).mkString))

    // todo use this and remove the . versions
    case "diesel" => Some(P.fromSmartTypedValue("diesel", new DieselParmSource(this)))

    case _ if s startsWith "diesel.realm.props" => new DieselParmSource(this).getp(s.replaceFirst("diesel.", ""))
    case _ if s startsWith "diesel.props.realm" => new DieselParmSource(this).getp(s.replaceFirst("diesel.", ""))

    case _ => None
  }


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

    dwix.dieselEnvFor(settings.flatMap(_.realm).mkString, au)
  }

  // user id if any
  def dieselUser(ctx:ECtx) = {
    val root = ctx.root
    val settings = root.engine.map(_.settings)
    val au = settings
      .flatMap(_.userId)

    au.mkString
  }

  /** source from settings - only if there's some value... otherwise base won't cascade */
  private def ps(name:String) : Option[P] =
    settings.postedContent.flatMap(_.getp(name))

  /** used for instance when perssisting a context - will overwrite the defautl */
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
  def remove (name: String): Option[P] = ???

  def getp   (name: String): Option[P] = name match {
    case "env" =>
      // first overrides, then settings and lastly current envList setting
      ctx
          .getp(name)
          .orElse(ctx.settings.env.map(P("diesel.env",_)))
          .orElse(Some(P("diesel.env", ctx.dieselEnv(ctx))))

    case "user" => Some(P("diesel.user", ctx.dieselUser(ctx)))
    case "isLocalhost" => Some(P.fromTypedValue("diesel.isLocalhost", razie.wiki.Services.config.isLocalhost))

    case "realm.props" | "props.realm" => {
      val p = Website.getRealmProps(ctx.root.settings.realm.mkString)
      Some(P.fromTypedValue("diesel.realm.props", p))
    }

    case _ if name startsWith "props.realm" => {
      val p = Website.getRealmProps(ctx.root.settings.realm.mkString)
      if (name == "props.realm") Some(P.fromTypedValue("diesel.props.realm", p))
      else {
        // todo need proper parm regex using AExprIdent
        val temp = name.replaceFirst("props.realm.", "")
        p.get(temp)
      }
    }

    case _ if name startsWith "realm.props" => {
      val p = Website.getRealmProps(ctx.root.settings.realm.mkString)
      if (name == "realm.props") Some(P.fromTypedValue("diesel.realm.props", p))
      else {
        // todo need proper parm regex using AExprIdent
        val temp = name.replaceFirst("realm.props.", "")
        p.get(temp)
      }
    }

    case _ => None
  }

  def put    (p: P): Unit = ???

  def listAttrs: List[P] = ???

}

/** todo hierarchical source for objects? */
class NextParmSource (ctx:DomEngECtx, pname:String, value:P) extends ParmSource {
  def remove (name: String): Option[P] = ???

  def getp   (name: String): Option[P] = name match {
      case pname => Some(value)
      case _ => None
    }

  def put    (p: P): Unit = ???

  def listAttrs: List[P] = ???

}

