/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model

import razie.Debug._
import razie.wiki.Services
import razie.{XpElement, XP, XpSolver}

object WikiPath {
  val debug = false;
}

/** base class - can have a topic wrapper or a link wrapper */
abstract class WWrapper(val cat: String) {
  /** make the proper ILink for this element */
  def mkLink: ILink

  /** get the associated page, if any */
  def page : Option[WikiEntry]
}

class WikiWrapper(val wid:WID) extends WWrapper(wid.cat) {
  lazy val w = Wikis.find(wid)

  /** links from page only, if defined */
  protected lazy val wilinks = (w.map(realw=> realw.ilinks.map {ilink =>
    if (ilink.wid.cat == "any") Wikis(wid.getRealm).findAnyOne(ilink.wid.name).map(w => ILink(w.wid, w.label))
    else Some(ilink)
  }.flatMap(_.toList) ++ lfrom ++ lto ++ BADlto))

  // TODO optimize
  def lfrom = w.toList.flatMap(realw=>Wikis.linksFrom(realw.uwid)).map(x=>new ILink(x.to.wid.get, x.to.nameOrId))
  def lto = w.toList.flatMap(realw=>Wikis.linksTo(realw.uwid)).map(x=>new ILink(x.from.wid.get, x.from.nameOrId))

  // TODO this is like extremely bad !!!
  protected def BADallPages = Wikis(wid.getRealm).pageNames("Category").flatMap(cat=>Wikis(wid.getRealm).pageNames(cat).flatMap(name=>Wikis(wid.getRealm).find(cat, name).toList)).toList

  protected def BADlto =
    if (Services.config.sitecfg("searchall").isDefined)
    BADallPages.filter(_.ilinks.exists(_.wid.formatted.name == wid.name)).map(realw=>new ILink(realw.wid, realw.label))
    else Nil
//  protected def BADlto = BADallPages.map(realw=>new ILink(realw.wid, realw.label))

  lazy val ilinks = wilinks.toList.flatMap(_.toList)

  def tags = w.map(_.contentTags).getOrElse(Map())

  def mkLink = ILink (wid, w.map(_.label).getOrElse(wid.name))

  override def toString = "WikiWrapper(" + wid + ")"

  override def page : Option[WikiEntry] = w
}

case class IWikiWrapper(val ilink: ILink) extends WikiWrapper(ilink.wid) {
  override def mkLink = ilink
  override def tags = w.map(_.contentTags).getOrElse(ilink.tags)

  override lazy val ilinks = wilinks.getOrElse(ilink.ilinks)
}

/**
 *  solver for wiki xp
 */
object WikiXpSolver extends XpSolver[WWrapper] {

  type T = WWrapper
  type CONT = List[WWrapper]
  type U = CONT

  val debug = WikiPath.debug

  override def children(root: T): (T, U) = {
    if(WikiPath.debug) println ("--CHILDREN("+root+")")
    val x=root match {
      case x: WikiWrapper => (x, children2(x, "*").toList.teeIf(debug,"C").asInstanceOf[U])
      case _              => throw new IllegalArgumentException()
    }
//    if(WikiPath.printTrace) println ("----CHILDREN("+root+") ="+x)
    x
  }

  override def getNext(o: (T, U), tag: String, assoc: String): List[(T, U)] = {
    if(WikiPath.debug) println("-getNext ("+o.toString+") ("+tag+")")
    // 1. all children of type
    o._2.asInstanceOf[List[WWrapper]].filter(zz => XP.stareq(zz.asInstanceOf[WWrapper].cat, tag)).teeIf(debug,"D").flatMap (_ match {
      // 2. wrap them
      case x: WikiWrapper => (x, children2(x, "*").toList.asInstanceOf[U]) :: Nil
    }).tee("E").toList
  }

  private def children2(node: WWrapper, tag: String): Seq[WWrapper] = {
    if(WikiPath.debug) println("---CHILDREN2 ("+node+") ("+tag+")")
    val x = node match {
      case b: WikiWrapper => {
        b.ilinks filter ("*" == tag || tag == _.wid.cat) map (n => Tuple2(n.wid.cat, n)) flatMap (t => t match {
          case (name: String, o: ILink) => IWikiWrapper(o) :: Nil
          case _ => Nil
        })
      }
      case what @ _ => throw new IllegalArgumentException("Unsupported json type here: " + what)
    }
//    if(WikiPath.printTrace) println("-----CHILDREN2 ("+node+") ("+tag+") =\n ------ "+x)
    x tee "C2"
  }

  override def getAttr(o: T, attr: String): String = {
    if(WikiPath.debug) println("-getAttr ("+o.toString+") ("+attr+")")
    val ret = o match {
      case o: IWikiWrapper => o.tags.get(attr).getOrElse("")
      case o: WikiWrapper  => o.w.flatMap(_.contentTags.get(attr)).getOrElse("")
      case _               => null
    }
    ret.toString
  }

  override def reduce(curr: Iterable[(T, U)], xe: XpElement): Iterable[(T, U)] =
    (xe.cond match {
      case null => curr.asInstanceOf[List[(T, U)]]
      case _    => curr.asInstanceOf[List[(T, U)]].filter(x => xe.cond.passes(x._1, this))
    }).filter(gaga => XP.stareq(gaga._1.asInstanceOf[WWrapper].cat, xe.name))

}

object TestWikiPath extends App {
  // val node = new WikiWrapper("Club", "Offroad_Ontario")
  val node = new WikiWrapper(WID("Calendar", "OO_XC_2012"))
  val noder = new WikiWrapper(WID("Race", "OO_XC_Mansfield,_by_HORRA_,_date_May_27,_2012"))
  val root = new razie.Snakk.Wrapper(node, WikiXpSolver)
  val rootr = new razie.Snakk.Wrapper(noder, WikiXpSolver)

  razie.Log.debug("debug")
  razie.Log.logger.debug("debug")
  razie.Log.logger.log.info("info")
  razie.Log.logger.log.error("error")

  println(razie.Log.logger.log)

  // println (root)
  // println ("+++++++++++++ "+root \ "*" \ "*")
//  println ("+++++++++++++ "+root \ "Race" )//\@ "date")
//  println ("+++++++++++++ "+root \ "*" \ "Race" )//\@ "date")
//  println ("+++++++++++++ "+rootr \ "Venue")
//  println ("+++++++++++++ "+root \ "Race" \ "Venue")
  println ("+++++++++++++ "+(root xpl "*/Race/Venue"))
//  println ("+++++++++++++ "+(root xpl "Race/Venue"))
//  println ("+++++++++++++ "+root \ "*" \ "Race" \ "Venue" \@ "loc")
}
