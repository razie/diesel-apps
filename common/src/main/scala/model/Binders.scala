/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package model

import play.api.mvc.PathBindable
import play.api.mvc.QueryStringBindable
import com.mongodb.casbah.Imports._
import razie.Log
import model._
import razie.wiki.Enc
import razie.wiki.model._

/** binders for codec pah objects in play */
object Binders {

  implicit def widPathBindable =
    new PathBindable[WID] {
      def bind(key: String, value: String): Either[String, WID] = {
//        Log.debug ("BINDER "+value)
        WID.fromPath(Enc.fromUrl(value)).map(Right(_)).getOrElse(Left("Oh-Uh"))
      }
      def unbind(key: String, wid: WID): String =
        wid.wpathFull
    }

  implicit def widQueryStringBindable(implicit sBinder: QueryStringBindable[String]) = new QueryStringBindable[WID] {
    def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, WID]] = {
      for {
        w  <- sBinder.bind(key, params)
      } yield {
        w match {
          case Right(ww) => WID.fromPath(Enc.fromUrl(ww)).map(x=>Right(x)).getOrElse (Left("Unable to bind bounds"))
          case _ => Left("Unable to bind bounds")
        }
      }
    }

    def unbind(key: String, wid: WID) =
      key+"=" +Enc.toUrl(wid.wpathFull)
   }

  implicit def cmwidPathBindable =
    new PathBindable[CMDWID] {
      def bind(key: String, value: String): Either[String, CMDWID] = {
//        Log.debug ("BINDER "+value)
        WID.cmdfromPath(Enc.fromUrl(value)).map(Right(_)).getOrElse(Left("Oh-Uh"))
      }
      def unbind(key: String, wid: CMDWID): String =
        wid.wid.get.wpathFull
    }

  implicit def cmwidQueryStringBindable(implicit sBinder: QueryStringBindable[String]) = new QueryStringBindable[CMDWID] {
    def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, CMDWID]] = {
      for {
        w  <- sBinder.bind(key, params)
      } yield {
        w match {
          case Right(ww) => WID.cmdfromPath(Enc.fromUrl(ww)).map(x=>Right(x)).getOrElse (Left("Unable to bind bounds"))
          case _ => Left("Unable to bind bounds")
        }
      }
    }

    def unbind(key: String, wid: CMDWID) =
      key+"=" +Enc.toUrl(wid.wid.get.wpathFull)
   }

//  implicit def widQueryStringBindable(implicit sBinder: QueryStringBindable[String]) = new QueryStringBindable[razie.wiki.model.WID] {
//    def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, razie.wiki.model.WID]] = {
//      for {
//        cat  <- sBinder.bind(key + ".cat", params)
//        name <- sBinder.bind(key + ".name", params)
//        section <- sBinder.bind(key + ".section", params)
//      } yield {
//        val p = sBinder.bind(key + ".p", params)
//        (cat, name) match {
//          case (Right(c), Right(n)) => p match {
//            case Some(Right(pp)) => Right(WID(c, n, Some(new ObjectId(pp)), section.fold((x=>None), Some(_))))
//            case _ => Right(WID(c, n, None, section.fold((x=>None), Some(_))))
//          }
//          case _ => Left("Unable to bind bounds")
//        }
//      }
//    }
//
//    def unbind(key: String, wid: razie.wiki.model.WID) =
//      key+".cat=" + wid.cat + "&" +
//      key+".name=" + wid.name + "&" +
//      key+".section=" + wid.section + "&" +
//      wid.parent.map(pp=>key+".p=" + pp.toString).getOrElse("")
//   }

}
