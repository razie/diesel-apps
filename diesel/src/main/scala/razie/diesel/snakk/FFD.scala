/**
  *  ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.diesel.snakk

import razie.diesel.dom.RDOM.P
import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

/** fixed format data - Mule style
  *
  * @param input leave empty if you want just formatting
  * @param schema FFD schema
  * @param emptyValues true will generate empty parms if missing in input
  */
class FFDPayload (input:String, schema:String, emptyValues:Boolean = false) {
  val (fields,err) = Try {
    (new FFDSpecParser).apply(schema)
  }.recover {
    case t:Throwable => (List.empty, t.getMessage)
  }.get

  /** parse the input */
  def parse : ParseResult[List[P]] = {
    val result = ParseResult.empty[List[P]]

    if(fields.isEmpty)
      if(err.isEmpty) result.collectError("schema is empty")
      else result.collectError(err)
    else {
      val m = fields.map { f =>
        Try {
          val s = input.substring(f.offset, f.length + f.offset)

          if(emptyValues || s.trim.length > 0)
            P(f.name, s, (if (f.ttype.toLowerCase != "string") f.ttype else ""))
          else
            P("","")
        }.recover {
          case t: Throwable => {
            result.collectError(s"${f.name} -> ${t.getClass.getSimpleName} : ${t.getMessage}")
            P("", "")
          }
        }.get

      }.filter(_.name.length > 0)

      result.collectResult(m)
    }

    result
  }

  /** parse into nice string representation */
  def show : String = {
    var errors = 0
    val m = fields.map {f=>
      val s = Try {
        input.substring(f.offset, f.offset+f.length)
      }. recover {
        case t:Throwable => {
          errors = errors+1
          t.getClass.getSimpleName + " : " + t.getMessage
        }
      }.get
      (f.name -> s)
    }

    (s"FIELDS ${fields.size} | ERRORS: $errors\n") + {
      m.map(e=> e._1 + ": "+e._2).mkString("\n")
    }

  }

  /** build the record */
  def build (parms:{def getp(name:String) : Option[P]}) : ParseResult[String] = {
    val result = ParseResult.empty[String]
    val res = fields.foldLeft("")((buffer, f) => {
      val s = parms.getp(f.name).map(_.currentStringValue).getOrElse("")
      val curr =
        if(s.length > f.length) s.substring(0, f.length-1)
        else s + " " * (f.length-s.length)
      buffer + curr
    }).mkString

    if(fields.isEmpty) {
      if (err.isEmpty) result.collectError("schema is empty")
      else result.collectError(err)
    }

    result.collectResult(res)
    result
    }
}

/** a FFD field definition */
case class FFDFieldSpec (name:String, ttype:String, length:Int, var offset:Int=0)

/** parse a FFD schema */
class FFDSpecParser extends RegexParsers {
  override def skipWhitespace = false

  def apply (input:String) : (List[FFDFieldSpec],String) = {
    var off = 0;

    def offsetsOf(fields:List[FFDFieldSpec]) = {
      fields.foldLeft(0)((o,e) => {e.offset=o; o+e.length})
      fields
    }

    parseAll(schema, input) match {
      case Success(value, _) => (offsetsOf(value), "")
      // don't change the format of this message
      case NoSuccess(msg, next) => (List.empty, s"[[CANNOT PARSE]] [${next.pos.toString}] : ${msg}")
    }
  }

  def schema = rep(propline) ~> rep(fields)

  def propline = prop <~ CRLF ^^ {
    case p => p
  }

  def CRLF = ("\r\n" | "\n") // normal eol

  // simple
  def sprop = "\\w*".r ~ ": *".r ~ ("[^,}\n\r]*".r) ^^ {
    case n ~ _ ~ v => (n,v.mkString.trim)
  }

  // js object
  def jsprop = "\\w*".r ~ ": *".r ~ ("\\{[^}]*\\}".r) ^^ {
    case n ~ _ ~ v => (n,v.mkString.trim)
  }

  def prop = jsprop | sprop

  def fields  = "- { " ~> rep(prop <~ " *[,}] *".r ) <~ CRLF ^^ {
    case l => {
      def f(s:String) = { l.find(_._1 == s).map(_._2).mkString }
      FFDFieldSpec(f("name").replace("'", ""), f("type"), f("length").toInt)
    }
  }

}

