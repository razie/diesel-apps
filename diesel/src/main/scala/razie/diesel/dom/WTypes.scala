/**  ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.dom

import org.json.{JSONArray, JSONObject}

/** expression and data types
  *
  * note these are physical or base types.
  */
object WTypes {
  final val NUMBER="Number"
  final val STRING="String"
  final val DATE="Date"
  final val REGEX="Regex"

  final val INT="Int"
  final val FLOAT="Float"
  final val BOOLEAN="Boolean"

  final val RANGE="Range"

  final val HTML="HTML"     // why not... html templates?

  final val XML="XML"
  final val JSON="JSON"     // see asJson
  final val OBJECT="Object" // same as json

  final val ARRAY="Array"   // pv.asArray

  final val BYTES="Bytes"

  final val EXCEPTION="Exception"
  final val ERROR="Error"

  final val UNKNOWN=""

  final val MSG = "Msg"   // a message (to call)
  final val FUNC = "Func" // a function (to call)

  final val UNDEFINED="Undefined" // same as null - it means it's missing, not that it has an empty value

  object Mime {
    final val appJson = "application/json"
    final val appText = "application/text"
    final val appXml = "application/xml"
    final val textPlain = "text/plain"
    final val textHtml = "text/html"
  }

  // see also P.fromTypedValue
  def typeOf (x:Any) = {
    val t = x match {
      case m: Map[_, _] => JSON
      case s: String => STRING
      case i: Int => NUMBER
      case f: Double => NUMBER
      case f: Float => NUMBER
      case l: List[_] => ARRAY
      case l: JSONArray => ARRAY
      case l: JSONObject => JSON
//      case l: EMsg => MSG
      case h @ _ => UNKNOWN
    }
    t
  }

  /** get corresponding content-type */
  def getContentType (ttype:String) = {
    val t = ttype match {
      case JSON | ARRAY => Mime.appJson
      case XML => Mime.appXml
      case HTML => Mime.textHtml
      case h @ _ if h.trim.length > 0 => h.trim // whatever came in - maybe it's valid :)
      case _ => Mime.textPlain
    }
    t
  }

  def isSubtypeOf (a:String, b:String) = {
    a.toLowerCase == b.toLowerCase || {
      a.toLowerCase == EXCEPTION && b.toLowerCase == ERROR
    }
  }
}


