/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package model

import play.api.mvc.PathBindable
import play.api.mvc.QueryStringBindable
import razie.wiki.Enc
import razie.wiki.model._

/** binders for codec path objects in play, String to CMDWID/WID and back */
object Binders {

  implicit def widPathBindable =
    new PathBindable[WID] {
      def bind(key: String, value: String): Either[String, WID] = {
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
}
