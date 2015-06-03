/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.parser

import org.bson.types.ObjectId
import razie.wiki.{Services, Enc}
import razie.{cdebug, cout, clog}

import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.util.matching.Regex.Match
import scala.util.parsing.combinator.RegexParsers
import scala.Option.option2Iterable
import scala.collection.mutable
import razie.wiki.model._

/**
 * sed like filter using Java regexp
 *
 *  example: from US to normal: Sed ("""(\d\d)/(\d\d)/(\d\d)""", """\2/\1/\3""", "01/31/12")
 *
 *  Essentially useless since plain "sss".replaceAll(..., "$1 $2...") works almost the same way..
 */
object SedWiki {
  val SEARCH = """search:?([^]]*)""".r
  val SEARCH2 = """q:?([^]]*)""".r
  val LIST = """list:?([^.]*\.)?([^]]*)""".r
  val USERLIST = """userlist:?([^]]*)""".r
  val ALIAS = """alias:([^\]]*)""".r
  val NORMAL = """(rk:)?([^|\]]*)([ ]*[|][ ]*)?([^]]*)?""".r
  val ROLE = """([^:]*::)?([^|\]]*)([ ]*[|][ ]*)?([^]]*)?""".r

  def apply(realm:String, repf: (String => String), input: String): Option[(String, Option[ILink])] = {
    var i: Option[ILink] = None

    input match {
      case SEARCH(nm) =>
        Some("""<a href="http://google.com/search?q=""" + Enc.toUrl(nm) + "\">" + nm + "</a>", None)
      case SEARCH2(nm) =>
        Some("""<a href="http://google.com/search?q=""" + Enc.toUrl(nm) + "\">" + nm + "</a>", None)

      case LIST(newr, cat) => Some({
        val newRealm = if(newr == null || newr.isEmpty) realm else newr.substring(0,newr.length-1)
        Wikis(newRealm).pageNames(cat).take(50).toList.sortWith(_ < _).map { p =>
          Wikis.formatWikiLink(realm, WID(cat, p).r(newRealm), p, p, None)
        }.map(_._1).mkString(" ")
      },
      None)

        //todo what the heck is this?
      case USERLIST(cat) => {
        // TODO can't see more than 20-
        Some((
          try {
            val up = razie.NoStaticS.get[WikiUser]
            val upp = up.toList.flatMap(_.myPages(realm, cat)).map(_.asInstanceOf[{ def wid: WID }])
            "<ul>" + upp.sortWith(_.wid.name < _.wid.name).take(20).map(_.wid).map { wid =>
              Wikis.formatWikiLink(realm, wid, Wikis(realm).label(wid).toString, Wikis(realm).label(wid).toString, None)
            }.map(_._1).map(x => "<li>" + x + "</li>").mkString(" ") + "</ul>"
          } catch {
            case e @ (_: Throwable) => {
              println(e.toString);
              "ERR Can't list userlist"
            }
          }, None))
      }

      case ALIAS(wpath) => {
        val wid = WID.fromPath(wpath)
        wid.map { w =>
          val f = Wikis.formatWikiLink(realm, w, w.name, w.name, None)
          ("Alias for " + f._1, f._2)
        }
      }

      case ROLE(role, wpath, _, label) => {
        val wid = WID.fromPath(wpath)
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
        val wid = WID.fromPath(wpath)
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

