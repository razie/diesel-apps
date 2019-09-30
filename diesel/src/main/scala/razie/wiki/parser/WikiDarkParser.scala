/**
  * ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  * )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  **/
package razie.wiki.parser

import razie.tconf.parser.{LazyAstNode, StrAstNode}
import razie.wiki.Services
import razie.wiki.model.{WikiEntry, WikiUser}

/** elemnts for dark/light backgrounds */
trait WikiDarkParser extends ParserBase {

  def darkHtml = wikiPropImgDark | htmlDark

  // todo remove this - relies on statics
  private def isDark(au:Option[WikiUser]) = !Services.config.oldisLight(au)

  def wikiPropImgDark: PS = "{{img" ~> """\.light|\.dark""".r ~ """[: ]""".r ~ """[^} ]*""".r ~ optargs <~ "}}" ^^ {
    case stype ~ _ ~ name ~ args => {
      LazyAstNode[WikiEntry,WikiUser] { (current, ctx) =>
        if(isDark(ctx.au) && stype.contains("dark") || !isDark(ctx.au) && stype.contains("light")) {
          val sargs = args.foldLeft(""){(c, a) => s""" $c ${a._1}="${a._2}" """}
          StrAstNode(s"""<img src="$name" $sargs />""")
        } else {
          StrAstNode("")
        }
      }
    }
  }

  // to not parse the content, use slines instead of lines
  def htmlDark: PS = "{{" ~> opt(".") ~ """html.dark|html.light""".r ~ "}}" ~ lines <~ ("{{/" ~ """html""".r ~ "}}") ^^ {
    case hidden ~ stype ~ _ ~ lines => {
      LazyAstNode[WikiEntry,WikiUser] { (current, ctx) =>
        if(isDark(ctx.au) && stype.contains("dark") || !isDark(ctx.au) && stype.contains("light")) {
          lines.fold(ctx)
        } else {
          StrAstNode.EMPTY
        }
      }
    }
  }
}
