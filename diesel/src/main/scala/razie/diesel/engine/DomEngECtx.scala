package razie.diesel.engine

import org.bson.types.ObjectId
import razie.diesel.dom.RDOM.P
import razie.diesel.dom.{DSpec, ECtx, RDomain, SimpleECtx}

/** specific root context for an enging instance
  *
  * it also has unique IDs and such, to help with tests ran continuously
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

  override def apply(name: String): String = overwritten.map(_.apply(name)).orElse(ps(name)).orElse(pu(name).map(_.dflt)).getOrElse(super.apply(name))

  override def getp(name: String): Option[P] =
    overwritten.flatMap(_.getp(name)).orElse(ps(name).map{v=>
      P(name,v)
    }).orElse(pu(name)).orElse(super.getp(name))

  override def put(p: P): Unit = overwritten.map(_.put(p)).getOrElse(super.put(p))

  override def putAll(p: List[P]): Unit = overwritten.map(_.putAll(p)).getOrElse(super.putAll(p))

  override def root: ECtx = overwritten.map(_.root).getOrElse(super.root)

  override def exists(f: scala.Function1[P, scala.Boolean]): scala.Boolean =
    overwritten.map(_.exists(f)).orElse(settings.postedContent.map(_.exists(f))).getOrElse(super.exists(f))

  // uniques
  val uid = P("UID", new ObjectId().toString)
  val millis = P("MILLIS", System.currentTimeMillis().toString)

  /** source some of the unique values to help rerun tests */
  def pu (s:String) : Option[P] = s match {
    case "UID" => Some(uid)
    case "MILLIS" => Some(millis)
    case "CURMILLIS" => Some(P("CURMILLIS", System.currentTimeMillis().toString))
    case _ => None
  }

  /** source from settings */
  private def ps(name:String) : Option[String] = settings.postedContent.flatMap(_.get(name))


  /** used for instance when perssisting a context - will overwrite the defautl */
  def overwrite(ctx: ECtx) =
    if (this != ctx)
      overwritten = Some(ctx)

}
