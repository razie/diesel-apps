/*  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine.nodes

import org.bson.types.ObjectId
import razie.Logging
import razie.diesel.dom.RDOM.{O, P}
import razie.diesel.dom.RDomain.DOM_LIST
import razie.diesel.dom.{RDomain, WikiDomain}
import razie.diesel.engine._
import razie.diesel.expr.ScopeECtx
import razie.diesel.model.DieselMsg
import razie.diesel.utils.{DomUtils, SpecCache}
import razie.tconf.{DSpec, TSpecRef, TagQuery}
import razie.wiki.admin.Autosave
import razie.wiki.model._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** engine prep utilities: load stories, parse DOMs etc */
object EnginePrep extends Logging {

  /** load all stories for reactor, either drafts or final and return the list of wikis */
  def loadStories(settings: DomEngineSettings, reactor: String, userId: Option[ObjectId], storyWpath: String) = {
    val uid = userId.getOrElse(new ObjectId())
    val pages =
      if (settings.blenderMode) {
        val list =
          settings.tagQuery.map { tagQuery =>
            // todo how can we optimize for large reactors: if it starts with "story" use the Story category?
            val tq = new TagQuery(tagQuery)

            // todo optimize
            val wl = (if (tq.ltags.contains("Story") || tq.ltags.contains("story"))
              catPages("Story", reactor)
            else
              Wikis(reactor).pages("*")
                )

            val x = wl.toList

            x.filter(w => tq.matches(w.tags))
          } getOrElse {
            // todo optimize
            // Wikis(reactor).pages("Story").toList
            Wikis(reactor).pages("*").filter(w => w.tags.contains("story")).toList
          }

        val maybeDrafts = list.map { p =>
          //         if draft mode, find the auto-saved version if any
          if (settings.draftMode) {
            val c = Autosave.find("wikie", p.wid.defaultRealmTo(reactor), uid).flatMap(_.get("content")).mkString
            if (c.length > 0) p.copy(content = c)
            else p
          } else p
        }
        maybeDrafts
      } else {
        val spw = WID.fromPath(storyWpath).flatMap(_.page).map(_.content).getOrElse(DomUtils.SAMPLE_SPEC)
        val specName = WID.fromPath(storyWpath).map(_.name).getOrElse("fiddle")
        val spec = Autosave.OR("wikie", WID.fromPathWithRealm(storyWpath, reactor).get, uid, Map(
          "content" -> spw
        )).apply("content")

        val page = new WikiEntry("Story", specName, specName, "md", spec, uid, Seq("dslObject"), reactor)
        List(page)
      }
    pages
  }

  /** all pages of category
    * - inherit ALL specs from mixins
    */
  def catPages(cat: String, realm: String, overwriteTopics: Boolean = false): List[WikiEntry] = {
    if ("Spec" == cat) {
      val w = Wikis(realm)
      val l = (w.pages(cat).toList ::: w.mixins.flattened.flatMap(_.pages(cat).toList))
      // distinct in order - so I overwrite mixins
      val b = ListBuffer[WikiEntry]()
      val seen = mutable.HashSet[WikiEntry]()
      for (x <- l) {
        if (!seen.exists(
          y => (overwriteTopics || !overwriteTopics && y.realm == x.realm) &&
              y.category == x.category &&
              y.name == x.name)) {
          b append x
          seen += x
        }
      }
      b.toList
    } else {
      Wikis(realm).pages(cat).toList
    }
  }

  /**
    * prepare an engine, loading an entire reactor/realm
    *
    * globalStory is dumped at root level, the other stories are scoped
    */
  def prepEngine(id: String,
                 settings: DomEngineSettings,
                 reactor: String,
                 iroot: Option[DomAst],
                 justTests: Boolean,
                 au: Option[WikiUser],
                 description:String,
                 startStory:Option[WikiEntry]=None,
                 useTheseStories: List[WikiEntry] = Nil,
                 endStory:Option[WikiEntry]=None,
                 addFiddles: Boolean = false) : DomEngine = {
    val uid = au.map(_._id)
        .orElse(settings.configUserId)
        .getOrElse(new ObjectId())

    // is there a current fiddle in this reactor/user?
    val wids = Autosave.OR("DomFidPath", WID("", "").r(reactor), uid, Map(
      "specWpath" -> """""",
      "storyWpath" -> """"""
    ))

    var storyWpath = wids("storyWpath")

    val stw =
      WID
        .fromPath(storyWpath)
        .flatMap(_.page)
        .map(_.content)
        .getOrElse(DomUtils.SAMPLE_STORY)

    val storyName = WID.fromPath(storyWpath).map(_.name).getOrElse("fiddle")

    val story = Autosave.OR("wikie", WID.fromPathWithRealm(storyWpath, reactor).get, uid, Map(
      "content" -> stw
    )).apply("content")

    val id = java.lang.System.currentTimeMillis().toString()

    val pages =
      if (settings.blenderMode) { // blend all specs and stories
        val d = catPages("Spec", reactor).toList.map { p =>
          //         if draft mode, find the auto-saved version if any
          if (settings.draftMode) {
            val a = Autosave.find("wikie", p.wid.defaultRealmTo(reactor), uid)
            val c = a.flatMap(_.get("content")).mkString
            if (c.length > 0) p.copy(content = c)
            else p
          } else p
        }
        d
      } else {
        val specWpath = wids("specWpath")
        val spw = WID.fromPath(specWpath).flatMap(_.page).map(_.content).getOrElse(DomUtils.SAMPLE_SPEC)
        val specName = WID.fromPath(specWpath).map(_.name).getOrElse("fiddle")
        val spec = Autosave.OR("wikie", WID.fromPathWithRealm(specWpath,reactor).get, uid, Map(
          "content" -> spw
        )).apply("content")

        val page = new WikiEntry("Spec", specName, specName, "md", spec, uid, Seq("dslObject"), reactor)
        List(page)
      }

    // from all the stories, we need to extract all the spec fiddles and add to the dom
    val specFiddles = useTheseStories.flatMap { p =>
      // add sections - for each fake a page
      // todo instead - this shold be in RDExt.addStoryToAst with the addFiddles flag
      p :: sectionsToPages(p, p.sections.filter(s => s.stype == "dfiddle" && (Array("spec") contains s.signature)))
    }

    // finally build teh entire fom

    val dom = (pages ::: specFiddles).flatMap(p =>
      SpecCache.orcached(p, WikiDomain.domFrom(p)).toList
    ).foldLeft(
      RDomain.empty
    )((a, b) => a.plus(b)).revise.addRoot


    //    stimer snap "2_parse_specs"

    val ipage = new WikiEntry("Story", storyName, storyName, "md", story, uid, Seq("dslObject"), reactor)
    val idom = WikiDomain.domFrom(ipage).get.revise addRoot

    //    stimer snap "3_parse_story"

    val root = iroot.getOrElse(DomAst("root", "root"))

    // start processing all elements
    val engine = DieselAppContext.mkEngine(dom, root, settings, ipage :: pages map WikiDomain.spec, description)

    val stories = if (!useTheseStories.isEmpty) useTheseStories else List(ipage)
    startStory.map { we =>
      logger.debug("PrepEngine globalStory:\n" + we.content)
      // main story adds to root, no scope wrappers - this is globals
      addStoryWithFiddlesToAst(engine, List(we), false, false, false)
    }

    addStoryWithFiddlesToAst(engine, stories, justTests, false, addFiddles)

    endStory.map { we =>
      logger.debug("PrepEngine endStory:\n"+we.content)
      // main story adds to root, no scope wrappers - this is globals
      addStoryWithFiddlesToAst(engine, List(we), false, false, false)
    }

    engine
  }

  final val FIDDLE = "fiddle"

  /* extract more nodes to run from the story - add them to root */
  def sectionsToPages(story: WikiEntry, sections: List[WikiSection]): List[WikiEntry] = {
    sections
      .map { sec =>
        // do NOT change the fiddle tag - is used elsewhere
        val newPage = new WikiEntry(story.wid.cat, story.wid.name + "#" + sec.name, FIDDLE, "md",
          sec.content,
          story.by, Seq("dslObject", FIDDLE), story.realm)
        newPage
      }
  }

  def hasFiddleStories (story:DSpec) =
    story.findSection(s => s.stype == "dfiddle" && (Array("story") contains s.tags)).isDefined

  /* extract more nodes to run from the story - add them to root */
  def listStoriesWithFiddles(stories: List[WikiEntry], addFiddles: Boolean = false) = {

    val allStories = if (!addFiddles) {
      stories
    } else {
      stories.flatMap { story =>
        // add sections - for each fake a page
        // todo instead - this shold be in RDExt.addStoryToAst with the addFiddles flag
        story :: sectionsToPages(story, story.sections.filter(s => s.stype == "dfiddle" && (Array("story") contains s.signature)))
      }
    }

    allStories
  }

  /* extract more nodes to run from the story - add them to root */
  def addStoryWithFiddlesToAst(engine: DomEngine, stories: List[WikiEntry], justTests: Boolean = false,
                               justMocks: Boolean = false, addFiddles: Boolean = false) = {

    val allStories = listStoriesWithFiddles(stories, addFiddles)

    addStoriesToAst(engine, allStories, justTests, justMocks, addFiddles)
  }

  /** nice links to stories in AST trees */
  case class StoryNode (path:TSpecRef) extends CanHtml with InfoNode {
    def x = s"""<a id="${path.wpath.replaceAll("^.*:", "")}"></a>""" // from wpath leave just name
    override def toHtml = x + s"""Story ${path.ahref.mkString}"""
    override def toString = "Story " + path.wpath
  }

  /* add a message */
  def addMsgToAst(root: DomAst, v : EMsg) = {
    val ast = DomAst(v, AstKinds.RECEIVED)
    root.appendAllNoEvents(List(ast))
    ast
  }

  /**
    * add all nodes from story and add them to root
    *
    * todo when are expressions evaluated?
    */
  def addStoriesToAst(engine: DomEngine, stories: List[DSpec], justTests: Boolean = false, justMocks: Boolean =
  false, addFiddles: Boolean = false) = {
    var lastMsg: Option[EMsg] = None
    var lastMsgAst: Option[DomAst] = None
    var lastAst: List[DomAst] = Nil
    var inSequence = true
    val root = engine.root

    def addMsg(v: EMsg) = {
      lastMsg = Some(v);
      // withPrereq will cause the story messages to be ran in sequence
      lastMsgAst = if (!(justTests || justMocks)) Some(DomAst(v, AstKinds.RECEIVED).withPrereq({
        if (inSequence) lastAst.map(_.id)
        else Nil
      })) else None // need to reset it
      lastAst = lastMsgAst.toList
      lastAst
    }

    def addMsgPas(v: EMsgPas) = {
      // withPrereq will cause the story messages to be ran in sequence
      lastMsgAst = if (!(justTests || justMocks)) Some(DomAst(v, AstKinds.RECEIVED).withPrereq({
        if (inSequence) lastAst.map(_.id)
        else Nil
      })) else None // need to reset it
      lastAst = lastMsgAst.toList
      lastAst
    }

    // if the root already had something, we'll continue sequentially
    // this is important for diesel.guardian.starts + diese.setEnv - otherwise tehy run after the tests
    lastAst = root.children.toList

    // add a single story
    def addStory (story:DSpec) = {
      var savedInSequence = inSequence

      story.parsed

      // add a node to represent the story, if multiple stories or fiddles
      if(stories.size > 1 || addFiddles)
        root.childrenCol appendAll {
          lastAst = List(DomAst(StoryNode(story.specRef), AstKinds.STORY).withPrereq(lastAst.map(_.id)).withStatus(DomState.SKIPPED))
          lastAst
        }

      if(stories.size > 1) root.childrenCol appendAll addMsg(EMsg("diesel.scope", "push"))

      // markup all constructs - make sure there are no unparsed elements:
      if (
        !hasFiddleStories(story) &&
            !story.tags.contains(FIDDLE) &&
            story.contentPreProcessed.contains("$expect")) {

        // optimized to only do this expensive part for test stories, not regular messages
        // expect for fiddles (line numbers are very off)
        val domList = story.collector.getOrElse(DOM_LIST, List[Any]()).asInstanceOf[List[Any]].reverse

        def findElemLine(row: Int) = {
          domList.exists { x =>
            x.isInstanceOf[HasPosition] && {
              val h = x.asInstanceOf[HasPosition]
              h.pos.isDefined && h.pos.get.line == row
            }
          }
        }

        // we could do all, but don't care much about other elements, just the tests...
        root appendAllNoEvents story.contentPreProcessed.lines.map(_.trim).zipWithIndex.filter(
//        storyAst appendAllNoEvents story.contentPreProcessed.lines.map(_.trim).zipWithIndex.filter(
          _._1.startsWith("$expect")).collect {

          case (line, row) if !findElemLine(row + 1) =>
            DomAst(EError(s"Unparsed line #$row: " + line, story.specRef.wpath), AstKinds.ERROR).withStatus(
              DomState.SKIPPED)
        }.toList
      }

      // add the actual elements
      root appendAllNoEvents RDomain.domFilter(story) {
        case o: O if o.name != "context" => List(DomAst(o, AstKinds.RECEIVED))

        case v: EMsg if v.entity == "ctx" && v.met == "storySync" => {
          inSequence = true
          Nil
        }

        case v: EMsg if v.entity == "ctx" && v.met == "storyAsync" => {
          inSequence = false
          Nil
        }

        case v: EMsg => addMsg(v)

        case v: EMsgPas => addMsgPas(v)

        case v: EVal => {
          // vals are also in sequence... because they use values in context
          lastAst = List(DomAst(v, AstKinds.RECEIVED).withPrereq(lastAst.map(_.id)))
          lastAst
        }

        case e: ExpectM if (!justMocks) => {
          // todo withGuard is connected in pexpect - is it really needed again?/
          lastAst = List(DomAst(
            e
                .withGuard(lastMsg.map(_.asMatch))
                .withTarget(lastMsgAst), AstKinds.TEST).withPrereq(lastAst.map(_.id)
          ))
          lastAst
        }

        case e: ExpectV if (!justMocks) => {
          // todo withGuard is connected in pexpect - is it really needed again?/
          lastAst = List(DomAst(
            e
                .withGuard(lastMsg.map(_.asMatch))
                .withTarget(lastMsgAst), AstKinds.TEST).withPrereq(lastAst.map(_.id)
          ))
          lastAst
        }

        case e: ExpectAssert if (!justMocks) => {
          // todo withGuard is connected in pexpect - is it really needed again?/
          lastAst = List(DomAst(
            e
                .withGuard(lastMsg.map(_.asMatch))
                .withTarget(lastMsgAst), AstKinds.TEST).withPrereq(lastAst.map(_.id)
          ))
          lastAst
        }

        // these don't wait - they don't run, they are collected together
        // todo this is a bit inconsistent - if one declares vals and then a mock then a val
        case v: ERule => List(DomAst(v, AstKinds.RULE))

        case v: EMock => List(DomAst(v, AstKinds.RULE))
      }.flatten


      // final scope pop if multiple stories added
      if(stories.size > 1) root.childrenCol appendAll addMsg(EMsg("diesel.scope", "pop"))

      inSequence = savedInSequence
    }

    stories.foreach (addStory)
  }

  /**
    * a simple strToDom parser
    */
  def strToDom[T] (str: String)(p:PartialFunction[Any,T]) : List[T] = {
    val story = new WikiEntry("Story", "temp", "temp", "md", str, new ObjectId(), Seq("dslObject"), "")

    story.parsed

    RDomain.domFilter(story) (p)
  }

  def msgFromStr (realm:String, s:String) = {
    strToDom (s) {
      case m : EMsg => m
    }
  }
}
