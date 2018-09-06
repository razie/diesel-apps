/**
 *  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import akka.actor.{ActorRef, ActorSystem, Props}
import razie.Logging
import razie.audit.Audit
import razie.diesel.dom.{RDomain, _}
import razie.tconf.DSpec

import scala.collection.mutable


/** an application static - engine factory and cache */
object DieselAppContext extends Logging {
  private var appCtx : Option[DieselAppContext] = None
  var engMap = new mutable.HashMap[String,DomEngine]()
  var refMap = new mutable.HashMap[String,ActorRef]()
  var router : Option[ActorRef] = None

  private var system : Option[ActorSystem] = None
  private var actorSystemFactory : Option[() => ActorSystem] = None

  def stopActor (id:String) = {
    refMap.get(id).map(getActorSystem.stop)
  }

  /** use this actor system - defaults to creating its own */
  def setActorSystem (s:ActorSystem) = {
    system = Some(s)
  }

  /** todo poor man's injection - use guice or stomething */
  def setActorSystemFactory (s:() => ActorSystem) = {
    actorSystemFactory = Some(s)
  }

  /** get current system, if set, or make a default one */
  def getActorSystem : ActorSystem = {
    synchronized {
      system.getOrElse {
        system = actorSystemFactory.map(_.apply()).orElse(Some(ActorSystem.apply()))
        system.get
      }
    }
  }

  /** when in a cluster, you need to set this on startup... */
  var localNode = "localhost"

  /** initialize the engine cache and actor infrastructure */
  def init (node:String="", app:String="") = {
    if(appCtx.isEmpty) appCtx = Some(new DieselAppContext(
      if(node.length > 0) node else localNode,
      if(app.length > 0) app else "default"
    ))

    val p = Props(new DomEngineRouter())
    val a = getActorSystem.actorOf(p)
    router = Some(a)
    a ! DEInit

    appCtx.get
  }

  /** the static version - delegates to factory */
  def mkEngine(dom: RDomain, root: DomAst, settings: DomEngineSettings, pages : List[DSpec]) = synchronized {
    val eng = ctx.mkEngine(dom, root, settings, pages)
    val p = Props(new DomEngineActor(eng))
    val a = getActorSystem.actorOf(p, name = "engine-"+eng.id)
    DieselAppContext.engMap.put(eng.id, eng)
    DieselAppContext.refMap.put(eng.id, a)

    if(serviceStarted) {
      a ! DEInit
    }

    eng
  }

  /** these actors won'y start processing unless this module/service is "started"
    * they put a lot of load and slow down the init of the website and it's acting eratic
    * so nothing is processed in the first 5 seconds or so...
    */
  @volatile var serviceStarted = false

  /** start all actors */
  def start = synchronized {
    if(!serviceStarted) {
      serviceStarted = true

      init() // make sure it was init

      refMap.values.foreach(_ ! DEInit)

      Audit.logdb("DEBUG", s"Starting all engines... ${refMap.size} engines waiting")
    }
  }

  def stop = {
  }

  def ctx = appCtx.getOrElse(init())

  def report = s"Engines: ${engMap.size} - running: ${engMap.values.filter(_.status == DomState.DONE).size}"
}

/** a diesel app context
  *
  * todo properly injecting these */
class DieselAppContext (node:String, app:String) {
  /** make an engine instance for the given AST root */
  def mkEngine(dom: RDomain, root: DomAst, settings: DomEngineSettings, pages : List[DSpec]) =
    new DomEngine(dom, root, settings, pages)
}


