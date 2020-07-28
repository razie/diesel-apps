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

  /** constants for complex types */
  object wt {
    final val NUMBER=WType("Number")
    final val STRING=WType("String")
    final val DATE=WType("Date")
    final val REGEX=WType("Regex")

    final val INT=WType("Int")
    final val FLOAT=WType("Float")
    final val BOOLEAN=WType("Boolean")

    final val RANGE=WType("Range")

    final val HTML=WType("HTML")     // why not... html templates?

    final val XML=WType("XML")
    final val JSON=WType("JSON")     // see asJson
    final val OBJECT=WType("Object") // java object - serialize via json

    final val SOURCE=WType("Source") // a source of values

    final val ARRAY=WType("Array")   // pv.asArray

    final val BYTES=WType("Bytes")

    final val EXCEPTION=WType("Exception")
    final val ERROR=WType("Error")

    final val UNKNOWN=WType("")

    final val MSG = WType("Msg")   // a message (to call)
    final val FUNC = WType("Func") // a function (to call)

    final val UNDEFINED=WType("Undefined") // same as null - it means it"s missing, not that it has an empty value

    final val EMPTY=WType("")
  }

  // constants for simple types -
  // todo should deprecate

  final val NUMBER="Number"
  final val STRING="String"
  final val DATE="Date"
  final val REGEX="Regex"

  final val INT="Int"
  final val FLOAT="Float"
  final val BOOLEAN="Boolean"

  final val RANGE="Range"

  final val HTML="HTML"     // why not... html templates? msg can create html results

  final val XML="XML"
  final val JSON="JSON"     // see asJson
  final val OBJECT="Object" // java object - serialize via json. todo: need a serialization framework

  final val SOURCE="Source" // a source of values

  final val ARRAY="Array"   // pv.asArray

  final val BYTES="Bytes"

  final val EXCEPTION="Exception"
  final val ERROR="Error"

  final val UNKNOWN=""

  final val MSG = "Msg"   // a message (to call)
  final val FUNC = "Func" // a function (to call)

  final val UNDEFINED="Undefined" // same as null - it means it's missing, not that it has an empty value

  final val CONTENT_TYPE="Content-type" // same as null - it means it's missing, not that it has an empty value

  /** just a few mime types */
  object Mime {
    final val appJson = "application/json"
    final val appText = "application/text"
    final val appXml = "application/xml"
    final val textPlain = "text/plain"
    final val textHtml = "text/html"
  }

  // see also P.fromTypedValue
  /** find the simple type of value, by type */
  def typeOf (x:Any) = {
    val t = x match {
      case m: collection.Map[_, _] => wt.JSON
      case s: String => wt.STRING
      case i: Int => wt.NUMBER
      case i: Long => wt.NUMBER
      case f: Double => wt.NUMBER
      case f: Float => wt.NUMBER
      case l: List[_] => wt.ARRAY
      case l: JSONArray => wt.ARRAY
      case l: JSONObject => wt.JSON
//      case l: EMsg => MSG
      case h @ _ => wt.UNKNOWN
    }
    t
  }

  /** get corresponding mime content-type */
  def getContentType (ttype:WType):String = {
    val t = ttype.name match {
      case JSON | ARRAY | OBJECT => Mime.appJson
      case XML => Mime.appXml
      case HTML => Mime.textHtml

      case NUMBER | STRING | DATE | REGEX | INT | FLOAT | BOOLEAN | RANGE => Mime.textPlain

      case h @ _ if h.contains("/") => h.trim // whatever came in - maybe it's valid :)
      case _ => Mime.textPlain
    }
    t
  }

  // todo is more fancy with types... schemas inherit
  /** check known static subtype hierarchies
    *
    * for dynamic hierarchies, based on DOM, use the RDomain
    */
  def isSubtypeOf (a:WType, b:WType):Boolean = {
    a.name.toLowerCase == b.name.toLowerCase || {
      a.name.toLowerCase == EXCEPTION && b.name.toLowerCase == ERROR
    }
  }

  /** check known static subtype hierarchies
    *
    * for dynamic hierarchies, based on DOM, use the RDomain
    */
  def isSubtypeOf (a:String, b:String):Boolean = {
    a.toLowerCase == b.toLowerCase || {
      a.toLowerCase == EXCEPTION && b.toLowerCase == ERROR
    }
  }

  /** check known static subtype hierarchies
    *
    * for dynamic hierarchies, based on DOM, use the RDomain
    */
  def isSubtypeOf (a:WType, b:String):Boolean = isSubtypeOf(a, WType(b))

  // todo deprecate when typecast not needed
//  def apply(wt:WType):WType = wt

  def mkString (t:WType, classLink:String=>String):String = {
    val col = (if (t.name.isEmpty) "" else ":" + (if (t.isRef) "<>" else ""))
    var res = if (t.schema == WTypes.UNKNOWN) col + classLink(t.name)
    else if (t.name == WTypes.OBJECT || t.name == WTypes.JSON) col + classLink(t.schema) + "(" + t.name + ")"
    else col + classLink(t.name) + (if (t.schema == WTypes.UNKNOWN) "" else "(" + t.schema + ")")

    res = res + t.wrappedType.map("[" + classLink(_) + "]").mkString
    res = res + t.mime.map("[" + _ + "]").mkString
    res
  }

}

/** an actual type, with a schema and a contained type
  *
  * with this marker now we can add more types...
  *
  * @param name is the WType
  * @param schema is either a DOMType or some indication of a schema in context
  * @param wrappedType is T in A[T]
  * @param mime is an optional precise mime to be represented in
  */
case class WType (name:String, schema:String = WTypes.UNKNOWN, wrappedType:Option[String]=None, mime:Option[String]=None, isRef:Boolean=false) {

  override def equals(obj: Any) = {
    obj match {
      case wt:WType => this.name == wt.name && this.schema == wt.schema
      case s:String => this.name == s
    }
  }

  def withSchema (s:String) = copy(schema=s)
  def hasSchema (s:String) = schema != "" && schema != WTypes.UNKNOWN

  def withMime (s:Option[String]) = copy(mime=s)
  def withRef (b:Boolean) = copy(isRef=b)

  override def toString = WTypes.mkString(this, identity)

  def isEmpty = name.isEmpty
  def nonEmpty = name.nonEmpty
}

