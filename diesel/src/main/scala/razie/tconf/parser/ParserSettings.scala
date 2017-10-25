/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.tconf.parser

object ParserSettings {
  /** debug the buildig of AST while pasing */
  var debugStates = false

  //todo load from some config source

//  WikiObservers mini {
//    case _:WikiConfigChanged =>
//      Services.config.sitecfg("ParserSettings.debugStates").foreach { s =>
//        debugStates = s.toBoolean
//      }
//  }

  //======================= forbidden html tags TODO it's easier to allow instead?

  //todo form|input allowed?

  final val hok = "abbr|acronym|address|a|b|blockquote|br|button|caption|div|dd|dl|dt|font|h1|h2|h3|h4|h5|h6|hr|i|img|li|p|pre|q|s|small|strike|strong|span|sub|sup|" +
    "table|tbody|td|tfoot|th|thead|tr|ul|u|input|form|textarea|select|style|label"
  final val hnok = "applet|area|base|basefont|bdo|big|body|center|cite|code|colgroup|col|" +
    "del|dfn|dir|fieldset|frame|frameset|head|html|iframe|ins|isindex|kbd|" +
    "legend|link|map|menu|meta|noframes|noscript|object|ol|" +
    "optgroup|option|param|samp|script|title|tt|var"

  final val mth1 = "Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec"
  final val mth2 = "January|February|March|April|May|June|July|August|September|October|November|December"
}


