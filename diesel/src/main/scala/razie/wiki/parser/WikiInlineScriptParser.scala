/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.wiki.parser

import razie.tconf.parser.StrAstNode

/** parse dsl, fiddles and code specific fragments */
trait WikiInlineScriptParser extends ParserBase {

  def codeWikiProps = wikiPropScript | wikiPropCall | wikiPropExpr

  // {{def name signature}}
  def wikiPropScript: PS = "{{" ~> "\\.?".r ~ """def|lambda|inline""".r ~ "[: ]".r ~ """[^:} ]*""".r ~ "[: ]".r ~ """[^}]*""".r ~ "}}" ~
    lines <~ ("{{/def}}" | "{{/lambda}}" |"{{/inline}}" | "{{/}}") ^^ {

    case hidden ~ stype ~ _ ~ name ~ _ ~ sign ~ _ ~ lines => {
      // inlines still need to be called with a call - but will be expanded right there
      if ("lambda" == stype)
        StrAstNode(s"`{{call:#$name}}`") // lambdas are executed right there...
      else if ("inline" == stype && hidden.length <= 0)
        StrAstNode(s"`{{call:#$name}}`") // lambdas are executed right there...
      else if(hidden.length <= 0)
        StrAstNode(s"`{{$stype:$name}}`") // defs are expanded in pre-processing and executed in display
      else StrAstNode.EMPTY
    }
  }

  def wikiPropCall: PS = "{{" ~> """call""".r ~ "[: ]".r ~ opt("""[^#}]*""".r) ~ "#" ~ """[^}]*""".r <~ "}}" ^^ {
    case stype ~ _ ~ page ~ _ ~ name => {
      StrAstNode("`{{" + stype + ":" + (page getOrElse "") + "#" + name + "}}`")
      // calls are executed in display
    }
  }

  def wikiPropExpr: PS = "{{" ~> ( "e" | "e.js" | "e.scala") ~ "[: ]".r ~ """[^}]*""".r <~ "}}" ^^ {
    case stype ~ _ ~ body => {
      StrAstNode("`{{" + stype + ":" + body + "}}`") // are evaluated on display - just check syntax here
      // calls are executed in display
    }
  }

}
