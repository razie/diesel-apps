/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.parser

import razie.diesel.dom.WikiDomain
import razie.wiki.Enc
import razie.wiki.model._

/**
 *  parse wiki links and [[...]] expressions
 */
object ParseWLink {
  val SEARCH = """search:?([^]]*)""".r
  val SEARCH2 = """q:?([^]]*)""".r
  val LIST = """list:?([^.]*\.)?([^]]*)""".r
  val ALIAS = """alias:([^\]]*)""".r
  val NORMAL = """(rk:)?([^|\]]*)([ ]*[|][ ]*)?([^]]*)?""".r       // [[rk.Topic:name]]
  val ROLE = """([^:]*::)?([^|\]]*)([ ]*[|][ ]*)?([^]]*)?""".r     // [[enabler::rk.Topic:name]]
  val BROWSE = """browse:([^|\]]*)([ ]*[|][ ]*)?([^]]*)?""".r      // [[browse:rk.Topic:name]]

  def apply(realm:String, repf: (String => String), input: String): Option[(String, Option[ILink])] = {
    var i: Option[ILink] = None

    input match {

      case SEARCH(nm) =>
        Some("""<a href="http://google.com/search?q=""" + Enc.toUrl(nm) + "\">" + nm + "</a>", None)

      case SEARCH2(nm) =>
        Some("""<a href="http://google.com/search?q=""" + Enc.toUrl(nm) + "\">" + nm + "</a>", None)

      case LIST(newr, cat) => Some({
        val newRealm = if(newr == null || newr.isEmpty) realm else newr.substring(0,newr.length-1)
        if(cat == "Domain")
          WikiDomain(newRealm).rdom.classes.values.take(50).toList.sortWith(_.name < _.name).map { c =>
            val x = Wikis.formatWikiLink(realm, WID("Category", c.name).r(newRealm), c.name, c.name, None)._1
            //emphasize the ones defined in this realm vs inherited from mixins
            if(Wikis(newRealm).cats.contains(c.name)) s"<b>$x</b>"
            else x
          }.mkString(" ")
        else
          Wikis(newRealm).pageNames(cat).take(50).toList.sortWith(_ < _).map { p =>
            Wikis.formatWikiLink(realm, WID(cat, p).r(newRealm), p, p, None)
          }.map(_._1).mkString(" ")
      },
      None)

      case ALIAS(wpath) => {
        val wid = WID.fromPath(wpath, realm)
        wid.map { w =>
          val f = Wikis.formatWikiLink(realm, w, w.name, w.name, None)
          ("Alias for " + f._1, f._2)
        }
      }

      case BROWSE(wpath, _, label) => {
        WID.fromPath(wpath, realm).map { w =>
          val lab = (if (label != null && label.length > 1) label else w.name)
          (s"""<a href="/wikie/browse/${w.formatted.wpath}">$lab</a>""" , Some(ILink(w, w.name)))
        }
      }

      case ROLE(role, wpath, _, label) => {
        val wid = WID.fromPath(wpath, realm)
        wid map (w => Wikis.formatWikiLink(
          realm, w,
          w.name,
          (if (label != null && label.length > 1) label else w.name),
          {
            if(role == null) None else Some(role.substring(0,role.length-2))
          },
          None))
      }

      case NORMAL(rk, wpath, _, label) => {
        val wid = WID.fromPath(wpath, realm)
        wid map (w => Wikis.formatWikiLink(
          realm, w,
          w.name,
          (if (label != null && label.length > 1) label else w.name),
        None,
          None,
          rk != null && rk.length > 0))
      }

      case _ => Some(ERR, i)
    }
  }

  val ERR = "[ERROR SYNTAX]"

  val patRep = """\\([0-9])""".r
}

