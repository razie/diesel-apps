/**
  *  ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 **/
package razie.diesel.engine

import akka.actor.{ActorRef, ActorSystem, Props}
import mod.diesel.model.exec.{EECtx, EESnakk}
import razie.Logging
import razie.audit.Audit
import razie.diesel.dom.RDomain
import razie.diesel.exec.{EEFormatter, EEFunc, EETest, Executors}
import razie.tconf.DSpec
import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext

/** a diesel application context - setup actor infrastructure etc
  *
  * todo properly injecting these
  * todo eventually supporting more contexts per JVM
  */
class DieselAppContext(node: String, app: String) {

  /** make an engine instance for the given AST root */
  def mkEngine(dom: RDomain,
               root: DomAst,
               settings: DomEngineSettings,
               pages: List[DSpec],
               description: String) =
    new DomEngine(dom, root, settings, pages, description)
}

/** an application static - engine factory, manager and cache */
object DieselAppContext extends Logging {
  val DIESEL_DISPATCHER = "diesel-dispatcher"

  private var _simpleMode = false

  @volatile private var appCtx: Option[DieselAppContext] = None

  implicit def executionContext =
    if(simpleMode) ExecutionContext.Implicits.global
    else getActorSystem.dispatchers.lookup(DIESEL_DISPATCHER)

  /** active engines -ussed for routing so weak concurrent control ok */
  val activeEngines = new TrieMap[String, DomEngine]()
  val activeActors = new TrieMap[String, ActorRef]()

  /** router - routes messages to individual engines */
  var router: Option[ActorRef] = None

  /** the actor system used */
  private var actorSystem: Option[ActorSystem] = None
  private var actorSystemFactory: Option[() => ActorSystem] = None

  /** when this is set, no multi-tenant or special thread pools and other features are used,
    * you don't need to initialize too much infrastructure
    */
  def simpleMode = _simpleMode

  def withSimpleMode() = {
    this._simpleMode = true
    this
  }

  def stopActor(id: String) = {
    activeActors.get(id).map(getActorSystem.stop)
  }

  /** use this actor system - defaults to creating its own */
  def mkExecutionContext () = {
    // default
    //    ExecutionContext.Implicits.global

    // custom
    ExecutionContext.fromExecutor(
      // flexible fork-join pool
      // new java.util.concurrent.ForkJoinPool(5)

      //If you want fixed size thread pool:
      java.util.concurrent.Executors.newFixedThreadPool(5)
    )
  }

  /** use this actor system - defaults to creating its own */
  def withActorSystem(s: ActorSystem) = synchronized {
    actorSystem = Some(s)
    this
  }

  /** todo poor man's injection - use guice or stomething */
  def withActorSystemFactory(s: () => ActorSystem) = synchronized {
    actorSystemFactory = Some(s)
    this
  }

  /** get current system, if set, or make a default one */
  def getActorSystem: ActorSystem = synchronized {
      actorSystem.getOrElse {
        actorSystem =
          actorSystemFactory
              .map(_.apply())
              .orElse(Some(ActorSystem.apply()))
        actorSystem.get
      }
    }

  /** when in a cluster, you need to set this on startup... */
  var localNode = "localhost"

  def initExecutors = {
    new EECtx ::
        new EESnakk ::
        new EETest ::
        new EEFunc ::
        new EEFormatter ::
        Nil map Executors.add
  }

  /** initialize the engine cache and actor infrastructure */
  def init(node: String = "", app: String = "") = {
    log("DieselAppContext.init")

    if (appCtx.isEmpty) {
      appCtx = Some(
        new DieselAppContext(
          if (node.length > 0) node else localNode,
          if (app.length > 0) app else "default"
        )
      )
    }

    val p = Props(new DomEngineRouter())
    val a = actorOf(p)
    router = Some(a)
    a ! DEInit

    appCtx.get
  }

  /** initialize the engine cache and actor infrastructure */
  def actorOf(props:akka.actor.Props, name: String="") = {
    val p = props //if(simpleMode) props else props.withDispatcher("xxt")//DIESEL_DISPATCHER)
    val a = if(name.isEmpty) getActorSystem.actorOf(p) else getActorSystem.actorOf(p, name)
    a
  }

  /** the static version - delegates to factory */
  def mkEngine(dom: RDomain,
               root: DomAst,
               settings: DomEngineSettings,
               pages: List[DSpec],
               description: String) = synchronized {

    val eng = ctx.mkEngine(dom, root, settings, pages, description)
    val p = Props(new DomEngineActor(eng))
    val a = actorOf(p, name = "engine-" + eng.id)

    DieselAppContext.activeEngines.put(eng.id, eng)
    DieselAppContext.activeActors.put(eng.id, a)

    if (serviceStarted) {
      a ! DEInit
    }

    eng
  }

  /** these actors won't start processing unless this module/service is "started"
    * they put a lot of load and slow down the init of the website and it's acting eratic
    * so nothing is processed in the first 5 seconds or so...
    */
  @volatile var serviceStarted = false

  /** start all actors */
  def start = synchronized {
    if (!serviceStarted) {
      serviceStarted = true

      init() // make sure it was init

      activeActors.values.foreach(_ ! DEInit)

      Audit.logdb(
        "DEBUG",
        s"Starting all engines... ${ activeActors.size} engines waiting"
      )
    }
  }

  def stop = {}

  def ctx = appCtx.getOrElse(init())

  def report =
    s"Engines: ${ activeEngines.size} - running: ${ activeEngines.values.filter(_.status == DomState.DONE).size}"
}

