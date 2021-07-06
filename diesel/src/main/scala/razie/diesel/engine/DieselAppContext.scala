/*   ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 **/
package razie.diesel.engine

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.util.Timeout
import razie.{Logging, clog}
import razie.audit.Audit
import razie.diesel.dom.RDomain
import razie.diesel.engine.exec._
import razie.tconf.DSpec
import razie.wiki.admin.GlobalData
import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

/** engine factory */
class DieselEngineFactory(node: String, app: String) {

  /** make an engine instance for the given AST root */
  def mkEngine(dom: RDomain,
               root: DomAst,
               settings: DomEngineSettings,
               pages: List[DSpec],
               description: String,
               correlationId: Option[String] = None) =
    new DomEngineV1(dom, root, settings, pages, description, correlationId)
}

/** a diesel application context - actor infrastructure, engine management, cache etc
  *
  * todo properly injecting these
  * todo eventually supporting more contexts per JVM, maybe?
  */
object DieselAppContext extends Logging {
  val DIESEL_DISPATCHER = "diesel-dispatcher"

  private var _simpleMode = false

  @volatile private var engineFactory: Option[DieselEngineFactory] = None

  implicit def executionContext =
    if (simpleMode) ExecutionContext.Implicits.global
    else {
      // todo this seems only used right now by the cron jobs - EEDieselCron
      // todo use for all diesel actors?
      val r = getActorSystem.dispatchers.lookup(DIESEL_DISPATCHER)
//      log("XXXXX " + r)
      r
    }

  /** active engines -ussed for routing so weak concurrent control ok */
  val activeEngines = new TrieMap[String, DomEngine]()
  val activeStreams = new TrieMap[String, DomStream]()
  val activeStreamsByName = new TrieMap[String, DomStream]()
  val activeActors = new TrieMap[String, ActorRef]()

  /** router - routes messages to individual engines */
  var router: Option[ActorRef] = None

  /** the actor system used */
  private var actorSystem: Option[ActorSystem] = None
  private var actorSystemFactory: Option[() => ActorSystem] = None

  /** when this is set, no multi-tenant or special thread pools and other features are used,
    * you don't need to initialize too much infrastructure and can run in the default akka executor
    */
  def simpleMode = _simpleMode

  /** when this is set, no multi-tenant or special thread pools and other features are used,
    * you don't need to initialize too much infrastructure and can run in the default akka executor
    */
  def withSimpleMode() = {
    this._simpleMode = true
    this
  }

  /** find stream by name */
  def findStream(name: String) = activeStreamsByName.get(name)

  def stopActor(id: String) = {
    activeActors.get(id).map(getActorSystem.stop)
  }

  /** use this actor system, for testing - defaults to creating its own */
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

  private def initExecutors = {
    new EECtx ::
        new EESnakk ::
        new EETest ::
        new EEFunc ::
        new EEFormatter ::
        new EEStreams ::
        Nil map Executors.add
  }

  /** initialize the engine cache and actor infrastructure */
  def init(node: String = "", app: String = "") = {
    log("DieselAppContext.init")

    if (engineFactory.isEmpty) {
      engineFactory = Some(
        new DieselEngineFactory(
          if (node.length > 0) node else localNode,
          if (app.length > 0) app else "default"
        )
      )
    }

//    log("XXXXXXX " + actorSystem.get.dispatcher)
//    log("XXXXXXX " + actorSystem.get.dispatcher.prepare())

    val p = Props(new DomEngineRouter())
    val a = actorOf(p)
    router = Some(a)
    a ! DEInit

    engineFactory.get
  }

  /** initialize the engine cache and actor infrastructure */
  def actorOf(props:akka.actor.Props, name: String="") = {
    // run the specs tests - some don't finish with the other dispatcher. I think it's the futures that use
    // the wrong execContext ?
    // run on 118 - somehow that one gets stuck on stream.consume ?
    val p = props //if (simpleMode) props else props.withDispatcher(DIESEL_DISPATCHER)
    val a = if (name.isEmpty) getActorSystem.actorOf(p) else getActorSystem.actorOf(p, name)
    a
  }

  /** the static version - delegates to factory */
  def mkEngine(dom: RDomain,
               root: DomAst,
               settings: DomEngineSettings,
               pages: List[DSpec],
               description: String,
               correlationId: Option[String] = None) = synchronized {

    val eng = ctx.mkEngine(dom, root, settings, pages, description, correlationId)
    val p = Props(new DomEngineActor(eng))
    val a = actorOf(p, name = "engine-" + eng.id)

    DieselAppContext.activeEngines.put(eng.id, eng)
    DieselAppContext.activeActors.put(eng.id, a)

    if (serviceStarted) {
      a ! DEInit
    }

    eng
  }

  /** the static version - delegates to factory */
  def mkStream(stream: DomStream) = synchronized {
    val p = Props(new DomStreamActor(stream))
    val a = actorOf(p, name = "stream-" + stream.id)

    GlobalData.dieselStreamsTotal.incrementAndGet()
    GlobalData.dieselStreamsActive.incrementAndGet()
    DieselAppContext.activeStreamsByName.put(stream.name, stream)
    DieselAppContext.activeStreams.put(stream.id, stream)
    DieselAppContext.activeActors.put(stream.id, a)

    if (serviceStarted) {
      a ! DEInit
    }

    stream
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

      initExecutors
      init() // make sure it was init

      activeActors.values.foreach(_ ! DEInit)

      Audit.logdb(
        "DEBUG",
        s"Starting all engines... ${activeActors.size} engines waiting"
      )
    }
  }

  /** send a message via the router */
  def !(message: Any)(implicit sender: ActorRef = Actor.noSender): Unit = {
    router.map(_ ! message)
    if (router.isEmpty) {
      throw new IllegalStateException("DieselAppContext.router not initialized?")
    }
  }

  /** send a message via the router */
  def stopStream(name: String): Unit = {
    import akka.pattern.ask
    implicit val timeout = Timeout(5 seconds)

//    activeStreamsByName
//        .get(name)
//        .map(x => {
//          activeActors.get(x.id).map(_ ? DESClean)
//            route(x.id, m); ""
//        })
//        .getOrElse(
//          clog << "DomEngine Router DROP STREAM message " + m
//        )
//  }


//    router.map(_ ? message)
//    if (router.isEmpty) {
//      throw new IllegalStateException("DieselAppContext.router not initialized?")
//    }
  }

  def stop = {}

  def ctx = engineFactory.getOrElse(init())

  def report =
    s"Engines: ${activeEngines.size} - running: ${activeEngines.values.filter(_.status != DomState.DONE).size}"
}

