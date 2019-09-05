/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.model

import com.mongodb.DBObject
import com.mongodb.casbah.Imports._
import razie.db.RazMongo
import razie.diesel.dom.{RDOM, RDomain, WikiDomain}
import razie.diesel.ext.{EMsg, ExpectM}
import razie.hosting.Website
import razie.tconf.TagQuery
import razie.wiki.model.Visibility.PUBLIC
import scala.Array.canBuildFrom
import scala.collection.mutable.ListBuffer

/** search utilities */
object WikiSearch {

  /**
   *
   * @param category
   * @param utags
   * @param page
   * @param qt - first array is and, second is or
   * @return
   */
  def filterTags (category:String, utags:String, page:Option[WikiEntry], qt:Array[Array[String]]) = {
    def checkT (b:String) = {
      utags.contains(b) ||
      b == "*" ||
      category.toLowerCase == b ||
      (b == "draft" && page.exists(_.isDraft)) ||
      (b == "public" && page.exists(_.visibility == PUBLIC))
    }

    qt.size > 0 &&
      qt.foldLeft(true)((a, b) => a && (
        if(b(0).startsWith("-")) ! checkT(b(0).substring(1))
        else b.foldLeft(false)((a, b) => a || checkT(b)))
      )
  }

  /** extract associations from the page
    * @return Tuple(left, middle, right, mkLink)
    */
  def extractAssocs (realm:String, page:Option[WikiEntry]) = {
    // website prop p
    def p(p:String) = Website.forRealm(realm).flatMap(_.prop(p)).mkString

    if(p("wbrowser.query") == "dieselMsg") {
      val domList = page.get.collector.getOrElse(RDomain.DOM_LIST, List[Any]()).asInstanceOf[List[Any]].reverse
      val colEnt = new ListBuffer[(RDOM.A, String)]()
      val colMsg = new ListBuffer[(RDOM.A, String)]()

      val mkl = (s : String) => ""
//      def mkLink (s:String) = routes.Wiki.wikiBrowse (s, path+"/"+s).toString()

      val all = domList.collect {
        case m:EMsg => {
          colEnt append ((
            RDOM.A(page.get.getLabel, page.get.name, m.entity, "me", "service"),
            ""
            ))
          colMsg append ((
            RDOM.A(page.get.getLabel, page.get.name, m.entity+"."+m.met, "me", "msg"),
            ""
          ))
        }
        case m:ExpectM => {
          colEnt append ((
            RDOM.A(page.get.getLabel, page.get.name, m.m.cls, "me", "service"),
            ""
          ))
          colMsg append ((
            RDOM.A(page.get.getLabel, page.get.name, m.m.cls + "." + m.m.met, "me", "msg"),
            ""
          ))
        }
      }
      (colEnt.distinct.toList.map(_._1), colMsg.distinct.toList.map(_._1), Nil, mkl)
    } else {
      // normal browse mode tagQuery
     val all = page.get.ilinks.distinct.collect {
        case link if link.wid.page.isDefined =>
          (RDOM.A(page.get.getLabel, page.get.name, link.wid.name, "me", link.role.mkString), link.wid.page.get.tags.mkString)
      }

      def qt(t:String) = {
        t.split("[/&]").filter(_ != "tag").map(_.split("[,|]"))
      }

      def filter(cat:String, tags:String, s:String) = {
        filterTags(cat, tags, page, qt(p(s)))
      }

      // todo beef it up - include topic and stuff
      val mkl = (s : String) => "/wiki/"+s
//      def mkLink (s:String) = routes.Wiki.wikiBrowse (s, path+"/"+s).toString()
//      def mkLink (s:String) = routes.Wiki.showWid (CMDWID(Option(s), none, "", ""), 1, "?").toString()

      (
        all.filter(x=>filter("", x._2, "wbrowser.left")).map(_._1),
        all.filter(x=>filter("", x._2, "wbrowser.middle")).map(_._1),
        all.filter(x=>filter("", x._2, "wbrowser.right")).map(_._1),
        mkl
      )
    }
  }


  /** search all topics  provide either q or curTags
    *
    * @param realm the realm
    * @param q - query string
    * @param scope - wpath of scope, if any
    * @param curTags = querytags
    * @return list[WikiEntry]
    */
  def getList(realm:String, q: String, scope:String, curTags:String="", max:Int=2000) : List[WikiEntry] = {
    //TODO optimize - index or whatever

    //    val realm = if("all" != irealm) getRealm(irealm) else irealm

    //TODO limit the number of searches - is this performance critical?

    val qi = if(q.length > 0 && q(0) == '-') q.substring(1).toLowerCase else q.toLowerCase
    val doesNotContain = q.length > 0 && q(0) == '-'

    def qnot(x:Boolean) = if(doesNotContain) !x else x

    //todo only filter if first is tag ?
    // array of array - first is AND second is OR
    val tagQuery = new TagQuery(curTags)
    val qt = tagQuery.qt

    //todo optimize: run query straight in the database ?
    def filter (u:DBObject) = {
      def uf(n:String) = if(u.containsField(n)) u.get(n).asInstanceOf[String] else ""

      def hasTags = tagQuery.matches(u)

      if (qi.length <= 0) // just a tag search
        hasTags
      else
        qnot(
          (qi.length > 1 && uf("name").toLowerCase.contains(qi)) ||
          (qi.length > 1 && uf("label").toLowerCase.contains(qi)) ||
          ((qi.length() > 3) && uf("content").toLowerCase.contains(qi))
        ) && hasTags
    }

    lazy val parent = WID.fromPath(scope).flatMap(x=>Wikis.find(x).orElse(Wikis(realm).findAnyOne(x.name)))

    val REALM = if("all" == realm) Map.empty[String,String] else Map("realm"->realm)
    val wikis =
      if(scope.length > 0 && parent.isDefined) {
        val p = parent.get

        def src (t:MongoCollection) = {
          for (
            u <- t.find(REALM ++ Map("parent" -> p._id)) if filter(u)
          ) yield u
        }.toList

        if(WikiDomain(realm).zEnds(p.category, "Child").contains("Item"))
          RazMongo.withDb(RazMongo("weItem").m, "query") (src)
        else
          RazMongo.withDb(RazMongo("WikiEntry").m, "query") (src)
      } else {
        RazMongo.withDb(RazMongo("WikiEntry").m, "query") { t =>
          for (
            u <- t.find(REALM) if filter(u)
          ) yield u
        }.toList
      }

    if (wikis.size == 1)
      wikis.map(WikiEntry.grated _)
    else {
      val wl1 = wikis.map(WikiEntry.grated _).take(max)
      //todo optimize - split and sort up-front, not as a separate step
      val wl2 = wl1.partition(w=> qnot(w.name.toLowerCase.contains(qi) || w.label.toLowerCase.contains(qi)))
      val wl = if(qi.length > 0) wl2._1.sortBy(_.name.length) ::: wl2._2 else wl1
      wl
    }
  }
}
