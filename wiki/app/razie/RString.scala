/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie

import java.net.{URLDecoder, URLEncoder}

object RString extends RStringBase
object RazString extends RStringBase

/** some string utilities */
class RStringBase {
  case class RazString(s: String) {
    /** split the string in 2 sections with a separator */
    private def getx (a:Array[String], i:Int) = if (a.size > i) a(i) else ""

    def split2(sep: String): (String, String) = {
      val a = s.split(sep)
      (getx(a,0), getx(a,1))
    }

    /** split the string in 3 sections with a separator */
    def split3(sep: String): (String, String, String) = {
      val a = s.split(sep)
      (getx(a,0), getx(a,1), getx(a,2))
    }

    /** split the string in 4 sections with a separator */
    def split4(sep: String): (String, String, String, String) = {
      val a = s.split(sep)
      (getx(a,0), getx(a,1), getx(a,2), getx(a,3))
    }

    /** split the string in 3 sections with 2 separators */
    def split3(sep1: String, sep2: String): (String, String, String) = {
      val b = s.split(sep1)
      val x1 = if (b.size > 0) b(0) else ""
      val a = if (b.size > 1) b(1).split(sep2) else Array("")
      (x1, getx(a,0), getx(a,1))
    }

    def toUrl = URLEncoder.encode(s, "UTF8")
    def fromUrl = URLDecoder.decode(s, "UTF8")

  }

  implicit def toRS(o: String) = { RazString(o) }
}
