/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.parser

import razie.tconf.parser.{PState, SState}

/** Created by raz on 2014-11-20.
 */
trait WikiParserNotes extends ParserBase {
  import WAST._
  
  def notesDotProps = dotPropAct | dotPropShare | dotPropEmail

  def dotPropAct: Parser[PState] = """^\.a """.r ~> """[^\n\r]*""".r ^^ {
    case value => SState(s"""<small><span style="color:red;font-weight:bold;">{{Action $value}}</span></small><br>""", Map("action" -> value))
  }

  def dotPropShare: Parser[PState] = """^\.shared """.r ~> """[^\n\r]*""".r ^^ {
    case value => SState(s"""<small><span style="color:red;font-weight:bold;">{{Share $value}}</span></small><br>""", Map("share" -> value))
  }

  def dotPropEmail: Parser[PState] = """^\.email """.r ~> """[^ \n\r]*""".r ^^ {
    case value => SState(s"""<small><span style="font-weight:bold;">{{email $value}}</span></small><br>""", Map("email" -> value))
  }
}
