/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.parser

import razie.tconf.parser.{FoldingContext, ListAstNode, LazyAstNode, TriAstNode, StrAstNode}
import razie.wiki.model.WikiSearch
import razie.wiki.Enc
import razie.wiki.model._
import razie.wiki.model.features._

import scala.Option.option2Iterable

/** basic wiki parser - this is a trait so you can mix it in, together with other parser extensions, into your own parser
  *
  * the major patterns recognized are:
  *
  * - markdown extensions
  * - escape most html tags
  * \[\[link to other wiki page\]\]
  * \[\[\[link to wikipedia page\]\]\]
  * {{markup and properties}}
  * {{{magic markup and properties}}}
  * .keyword markup   // dot first char
  * {{xxx multiline markup}}
  * ...
  * {{/xxx}}
  * ::beg multiline markup
  * ...
  * ::end
  *
  *
  * Each parsing rule creates a SState, which are flattened at the end to both update the WikiEntry as well as create the
  * String representation of it - so you can be lazy in the SState.
  */
trait WikiParserT extends WikiParserMini with CsvParser {
  import WAST._

  def lazyt(f:(StrAstNode, FoldingContext[WikiEntry,WikiUser]) => StrAstNode) =
    LazyAstNode[WikiEntry, WikiUser] (f)

  //======================= {{name:value}}

  // this is used when matching a link/name
  override protected def wikiPropsRep: PS = rep(wikiPropMagicName | wikiPropByName | wikiPropWhenName |
    wikiPropWhereName | wikiPropLocName | wikiPropRoles | wikiProp |
    xstatic) ^^ {
    // LEAVE this as a SState - don't make it a LState or you will have da broblem
    case l => StrAstNode(l.map(_.s).mkString, l.flatMap(_.props).toMap, l.flatMap(_.ilinks))
  }

  // this is used for contents of a topic
  override protected def wikiProps: PS =
    moreWikiProps.foldLeft(
    wikiPropISection | wikiPropMagic | wikiPropBy | wikiPropWhen | wikiPropXp | wikiPropXmap | wikiPropWhere |
    wikiPropLoc | wikiPropRoles | wikiPropAttrs | wikiPropAttr | wikiPropWidgets | wikiPropCsv | wikiPropCsv2 |
    wikiPropTable | wikiPropSection | wikiPropHtml | wikiPropImg | wikiPropVideo | wikiPropQuery |
    wikiPropCode | wikiPropField | wikiPropRk | wikiPropFeedRss | wikiPropTag | wikiPropExprS |
    wikiPropRed | wikiPropAlert | wikiPropLater | wikiPropHeading | wikiPropFootref | wikiPropFootnote |
    wikiPropIf | wikiPropJs | wikiPropVisible | wikiPropUserlist
    )((x,y) => x | y) | wikiProp

  override protected def dotProps: PS = moreDotProps.foldLeft(dotPropTags | dotPropName )((x,y) => x | y) | dotProp

  private def wikiPropMagic: PS = "{{{" ~> """[^}]*""".r <~ "}}}" ^^ {
    case value => {
      val p = parseAll(dates, value)
      if (p.successful) {
        StrAstNode("""{{Date %s}}""".format(value), Map("date" -> value))
      } else {
        StrAstNode("""{{??? %s}}""".format(value), Map("magic" -> value))
      }
    }
  }
  def wikiPropMagicName: PS = """{{{""" ~> """[^}]*""".r <~ """}}}""" ^^ {
    case value => {
      val p = parseAll(dates, value)
      if (p.successful) {
        StrAstNode("""{{date %s}}""".format(value), Map("date" -> value))
      } else {
        StrAstNode("""{{??? %s}}""".format(value), Map("magic" -> value))
      }
    }
  }

  def dotPropTags: PS = """^\.t """.r ~> """[^\n\r]*""".r  ^^ {
    case value => StrAstNode("", Map("inlinetags" -> value)) // hidden
  }

  def dotPropName: PS = """^\.n """.r ~> """[^\n\r]*""".r  ^^ {
    case value => StrAstNode(s"""<small><span style="font-weight:bold;">$value</span></small><br>""", Map("name" -> value))
  }

  private def wikiPropHeading: PS = """^#+ +""".r ~ """[^\n\r]*""".r  ^^ {
    case head ~ name => {
      val u = Enc toUrl name.replaceAll(" ", "_")
      StrAstNode(s"""<a name="$u"></a>\n$head $name""") // hidden
    }
  }

  private def wikiPropByName: PS = ("\\{\\{[Bb]y[: ]+".r | "\\{\\{[Cc]lub[: ]+".r) ~> """[^}]*""".r <~ "}}" ^^ {
    case place => StrAstNode("""{{by %s}}""".format(place), Map("by" -> place))
  }

  private def wikiPropBy: PS = ("\\{\\{[Bb]y[: ]+".r | "\\{\\{[Cc]lub[: ]+".r) ~> """[^}]*""".r <~ "}}" ^^ {
    case place => StrAstNode("{{by " + parseW2("""[[Club:%s]]""".format(place)).s + "}}", Map("club" -> place), ILink(WID("Club", place), place) :: Nil)
  }

  private def wikiPropWhere: PS = ("\\{\\{where[: ]".r | "\\{\\{[Aa]t[: ]+".r | "\\{\\{[Pp]lace[: ]+".r | "\\{\\{[Vv]enue[: ]+".r) ~> """[^}]*""".r <~ "}}" ^^ {
    case place => StrAstNode("{{at " + parseW2("""[[Venue:%s]]""".format(place)).s + "}}", Map("venue" -> place), ILink(WID("Venue", place), place) :: Nil)
  }

  private def wikiPropWhereName: PS = ("\\{\\{where[: ]".r | "\\{\\{[Aa]t[: ]+".r | "\\{\\{[Pp]lace[: ]+".r | "\\{\\{[Vv]enue[: ]+".r) ~> """[^}]*""".r <~ "}}" ^^ {
    case place => StrAstNode("""{{at %s}}""".format(place), Map("venue" -> place), ILink(WID("Venue", place), place) :: Nil)
  }

  private def wikiPropLoc: PS = "{{" ~> "loc" ~> """[: ]""".r ~> """[^}:]*""".r ~ ":".r ~ """[^}]*""".r <~ "}}" ^^ {
    case what ~ _ ~ loc => {
      if ("ll" == what)
        StrAstNode("""{{[Location](http://maps.google.com/maps?ll=%s&z=15)}}""".format(loc), Map("loc" -> (what + ":" + loc)))
      else if ("s" == what)
        StrAstNode("""{{[Location](http://www.google.com/maps?hl=en&q=%s)}}""".format(loc.replaceAll(" ", "+")), Map("loc" -> (what + ":" + loc)))
      else if ("url" == what)
        StrAstNode("""{{[Location](%s)}}""".format(loc), Map("loc" -> (what + ":" + loc)))
      else
        StrAstNode("""{{Unknown location spec: %s value %s}}""".format(what, loc), Map("loc" -> (what + ":" + loc)))
    }
  }

  private def wikiPropLocName: PS = "{{" ~> "loc" ~> """[: ]""".r ~> """[^:]*""".r ~ ":".r ~ """[^}]*""".r <~ "}}" ^^ {
    case what ~ _ ~ loc => {
      StrAstNode("""{{at:%s:%s)}}""".format(what, loc), Map("loc:" + what -> loc))
    }
  }

  private def wikiPropWhen: PS = ("\\{\\{when[: ]".r | "\\{\\{[Oo]n[: ]+".r | "\\{\\{[Dd]ate[: ]+".r) ~> """[^}]*""".r <~ "}}" ^^ {
    case date => {
      val p = parseAll(dates, date)
      if (p.successful) {
        StrAstNode("""{{Date %s}}""".format(date), Map("date" -> date))
      } else {
        StrAstNode("""{{Date ???}}""".format(date), Map())
      }
    }
  }

  private def wikiPropWhenName: PS = ("\\{\\{when[: ]".r | "\\{\\{[Oo]n[: ]+".r | "\\{\\{[Dd]ate[: ]+".r) ~> """[^}]*""".r <~ "}}" ^^ {
    case date => {
      val p = parseAll(dates, date)
      if (p.successful) {
        StrAstNode(s"""{{date $date}}""", Map("date" -> date))
      } else {
        StrAstNode(s"""$date??""", Map())
      }
    }
  }

  private def wikiPropFootnote : PS = "{{footnote" ~> """[: ]""".r ~> """[^:}]*""".r ~ opt("[: ]".r ~> """[^}]*""".r) <~ "}}" ^^ {
    case name ~ text => s"""<span id="$name"><sup>$name</sup></span>"""+text.map(s=>
        s"""<small>$s</small>"""
    ).mkString
  }

  private def wikiPropFootref : PS = "{{footref" ~> """[: ]""".r ~> """[^:}]*""".r <~ "}}" ^^ {
    case name => s"""<a href="#$name"><sup>$name</sup></a>"""
  }

  // reused
  private def a(name:String, kind:String,d:String="") =
    StrAstNode(s"Attr: <b>$name</b>", Map("attr:" + name -> kind))

  private def wikiPropAttrs: PS = "{{attrs" ~> """[: ]""".r ~> """[^:}]*""".r <~ "}}" ^^ {
    case names => {
      ListAstNode(StrAstNode("Attrs:") :: names.split(",").map(name=>a(name, "")).toList )
    }
  }

  private def wikiPropAttr: PS = "{{attr" ~> """[: ]""".r ~> """[^:}]*""".r ~ opt(":".r ~> """[^}]*""".r) <~ "}}" ^^ {
    case name ~ kind => {
      a (name, kind.getOrElse(""))
    }
  }

  private def wikiPropRoles: PS = "{{roles" ~> """[: ]+""".r ~> """[^:]*""".r ~ "[: ]+".r ~ """[^}]*""".r <~ "}}" ^^ {
    case cat ~ _ ~ how => {
      val r = if(Wikis.RK == realm) "Category" else realm+".Category"
      val cats = "<b>"+parseW2(s"[[$r:$cat | $cat]]").s+"</b>"

      StrAstNode(
        how match {
          case "Child"     =>  s"{{Has $cats(s)}}"
          case "Parent"    =>  s"{{Owned by $cats(s)}}"
          case "Spec"      =>  s"{{Specified by $cats(s)}}"
          case "SpecFor"   =>  s"{{Specification for $cats(s)}}"
          case "Assoc"     =>  s"{{Associated to $cats(s) as $how}}"
          case _           =>  s"{{Can link from $cats(s) as $how}}"
        },
        Map("roles:" + cat -> how))
    }
  }

  private def wikiPropUserlist: PS = "{{userlist" ~> """[: ]""".r ~> opt("[^.]*\\.".r) ~ "[^}]*".r <~ "}}" ^^ {
    case newr ~ cat => {
      // TODO can't see more than 20-
      val newRealm = if(newr.isEmpty) realm else newr.get.substring(0,newr.get.length-1)
      lazyt { (current, ctx) =>
        val res = try {
          val up = ctx.au
          val uw = up.toList.flatMap(_.myPages(newRealm, cat))
          val upp = uw.map(_.asInstanceOf[ {def wid: WID}])
          //            s"<!-- ($realm : $newRealm) ${upp.map(_.wid.wpath).mkString} ..... ${upp.map(_.wid.realm).mkString} -->" +
          "<ul>" +
            upp.sortWith(_.wid.name < _.wid.name).take(20).map(_.wid).map { wid =>
              Wikis.formatWikiLink(realm, wid, Wikis(realm).label(wid).toString, Wikis(realm).label(wid).toString, None)
            }.map(_._1).map(x => "<li>" + x + "</li>").mkString(" ") + "</ul>"
        } catch {
          case e@(_: Throwable) => {
            println(e.toString);
            "ERR Can't list userlist"
          }
        }
        StrAstNode(res)
      }
    }
  }

  // just a nice badge for RK
  private def wikiPropRk: PS = "{{" ~> ("rk" | "wiki" | "ski") ~ opt("[: ]".r ~> """[^}]*""".r) <~ "}}" ^^ {
    case rk ~ what => {
      rk match {
        case "rk" =>  what match {
          case Some("member") =>
            StrAstNode("""<a class="badge badge-warning" href="http://rk.dieselapps.com/wiki/Admin:Member_Benefits">RacerKidz</a>""")
          case Some("club") =>
            StrAstNode("""<a class="badge badge-warning" href="http://rk.dieselapps.com/wiki/Admin:Club_Hosting">RacerKidz</a>""")
          case _ =>
            StrAstNode("""<a class="badge badge-warning" href="http://rk.dieselapps.com">RacerKidz</a>""")
        }
        case "wiki" =>
          StrAstNode("""<a class="badge badge-warning" href="http://wiki.dieselapps.com">DieselApps</a>""")
        case "ski" =>
          StrAstNode("""<a class="badge badge-warning" href="http://www.effectiveskiing.com">EffectiveSkiing</a>""")
        case _ =>
          StrAstNode("""<a class="badge badge-warning" href="http://www.dieselapps.com">DieselApps</a>""")
      }
    }
  }

  private def wikiPropRed: PS = "{{" ~> "red" ~> opt("[: ]".r ~> """[^}]*""".r) <~ "}}" ^^ {
    case what => StrAstNode(s"""<span style="color:red;font-weight:bold;">${what.mkString}</span>""")
  }

  private def wikiPropLater: PS = "{{" ~> "later" ~> "[: ]".r ~> """[^ :]*""".r ~ "[: ]".r ~ """[^}]*""".r <~ "}}" ^^ {
    case id~ _ ~ url => StrAstNode(Wikis.propLater(id, url))
  }

  private def wikiPropWidgets: PS = "{{" ~> "widget[: ]".r ~> "[^: ]+".r ~ optargs <~ "}}" ^^ {
    case name ~ args => {
      lazyt { (current, ctx) =>
        findWidget(name, ctx.we)
          .map(expandWidget(args)(_))
          .getOrElse(StrAstNode(""))
      }
    }.cacheOk
  }

  private def findWidget (name:String, we:Option[WikiEntry]) : Option[String] = {
    val wid = WID("Admin", "widget_" + name)
    val widl = WID("Admin", "widget_" + name.toLowerCase)

      // todo cache this

    we.flatMap(_.findSection(name)).map(_.content).orElse(
      Wikis(realm).find(wid).orElse(
        Wikis(realm).find(widl)).orElse(
        Wikis.rk.find(wid)).map(_.content)
    )
  }

  private def expandWidget (args:List[(String,String)])(content:String) = {
      StrAstNode(
          args.foldLeft(content)((c, a) => c.replaceAll(a._1, a._2))
      )
  }

  //======================= forms

  private def wikiPropField: PS = "{{" ~> "f:" ~> "[^ :}]+".r ~ optargs <~ "}}" ^^ {
    case name ~ args => {
      lazyt { (current, ctx) =>
        ctx.we.foreach { we =>
          we.fields.put(name, FieldDef(name, "", args.map(t => t).toMap))
        }
        StrAstNode("`{{{f:%s}}}`".format(name))
      }
    }.cacheOk
  }

  // to not parse the content, use slines instead of lines
  /** {{section:name}}...{{/section}} */
  def wikiPropSection: PS = "{{" ~> opt(".") ~ """section|template|properties""".r ~ "[: ]".r ~ """[^ :}]*""".r ~ opt("[: ]".r ~ """[^}]*""".r) ~ "}}" ~ lines <~ ("{{/" ~ """section|template|properties""".r ~ "}}".r) ^^ {
    case hidden ~ stype ~ _ ~ name ~ sig ~ _ ~ lines => {
      val signature = sig.map(_._2).getOrElse("")
      //todo complete this - sections to use AST as well
      lazyt { (current, ctx) =>
        ctx.we.foreach{w=>
          // todo why can't I collect here?
          // todo note that more tags are parsed in the WikiEntry, like def/inline/lambda etc
          //          w.collectedSections += WikiSection(ctx.we.get, stype, name, signature, lines.toString)
        }
        hidden.map(x => StrAstNode.EMPTY) getOrElse {
          if(stype == "template" && ctx.we.isDefined)
          // hide it
            TriAstNode(s"""`{{$stype $name:$signature}}` (<small><a href="${ctx.we.get.wid.urlRelative}#$name">view</a></small>)<br>""", "", "").fold(ctx)
          else if ("properties" == stype || "properties" == name)
          // show it
            TriAstNode(s"`{{$stype $name:$signature}}`<br><pre>", lines, s"</pre><br>`{{/$stype}}` ").fold(ctx)
          else
          // show it
            TriAstNode(s"`{{$stype $name:$signature}}`<br>", lines, s"<br>`{{/$stype}}` ").fold(ctx)
        }
      }.cacheOk
    }
  }

  // to not parse the content, use slines instead of lines
  /** {{html}}...{{/html}} */
  def wikiPropHtml: PS = "{{" ~ """html""" ~ "}}" ~> slines <~ ("{{/" ~ """html""" ~ "}}".r) ^^ {
    // todo only allow this for localhost or paid realms, not free realms? or admins etc?
    // todo why have this and the escaped htmls (like scripts etc)
    case lines => {
      lines
    }
  }

//  /** {{FAU}}...{{/FAU}} */
//  def wikiPropFAU: PS = "{{" ~> "FAU[: ]".r ~ opt("[^}]+".r) ~ " *\\}\\}".r ~ lines <~ ("{{/" ~ """FAU""".r ~ " *}}".r) ^^ {
//    case stype ~ attrs ~ _ ~ lines => {
//      lazys ctx) =>
//        if(api.wix) RState("", lines, "").fold(ctx)
//        else SState.EMPTY
//      }
//    }
//  }

  /** {{alert.color}}...{{/alert}} */
  def wikiPropAlert: PS = "{{" ~> "alert[: ]+".r ~> """green|blue|yellow|red|black""".r ~ " *".r ~ opt("[^}]+".r) ~ " *\\}\\}".r ~ lines <~ ("{{/" ~ """alert""".r ~ " *}}".r) ^^ {
    case stype ~ _ ~ attrs ~ _ ~ lines => {
      val color = stype match {
        case "green" => "success"
        case "blue" => "info"
        case "yellow" => "warning"
        case "red" => "danger"
        case "black" => "black"
      }
      // todo someone mangles the quotes if this is just {{}}
      TriAstNode(s"""{{div class="alert alert-$color" ${attrs.mkString} }}""", lines, s"{{/div}}")
    }
  }

  def wikiPropIf: PS = "{{" ~> "if[: ]".r ~> ".*(?=\\}\\})".r ~ "\\}\\}".r ~ lines <~ ("{{/" ~ """if""".r ~ " *}}".r) ^^ {
    case expr ~ _ ~ lines => {
      lazyt { (current, ctx) =>
        ctx.we.map { we =>
          //          val res = ctx.eval(, expr)
          val res = Wikis.runScript(expr, "js", ctx.we, ctx.au)
          if(res == "true") lines.fold(ctx)
          else StrAstNode.EMPTY
        } getOrElse {
          StrAstNode.EMPTY
        }
      }
    }
  }

  /**
    * js expressions, evaluated in the context of the api.wix
    */
  def wikiPropJs: PS = "{{" ~> "js[: ]".r ~> ".*(?=\\}\\})".r <~ " *\\}\\}".r ^^ {
    case expr => {
      lazyt { (current, ctx) =>
        ctx.we.map { we =>
          val res = Wikis.runScript(expr, "js", ctx.we, ctx.au)
          StrAstNode(res)
        } getOrElse {
          StrAstNode.EMPTY
        }
      }
    }
  }

  def wikiPropVisible: PS = "{{" ~> "visible[: ]".r ~> "[^ }:]+".r ~ " *".r ~ opt("[^}]+".r) ~ " *\\}\\}".r ~ lines <~ ("{{/" ~ """visible""".r ~ " *}}".r) ^^ {
    case expr ~ _ ~ attrs ~ _ ~ lines => {
      lazyt { (current, ctx) =>
        ctx.we.map { we =>
          var desc = attrs.map("("+ _ +")").getOrElse("<small>("+lines.fold(ctx).s.split("(?s)\\s+").size +" words)</small>")
          if(ctx.au.exists(_.hasMembershipLevel(expr)))
            StrAstNode(
              s"""{{div class="alert alert-success"}}""" + s"<b>Member-only content/discussion begins</b> ($expr)" + s"{{/div}}" +
                lines.fold(ctx).s
            )
          else if(expr != "Moderator")
            StrAstNode(s"""{{div class="alert alert-danger"}}""" + s"<b>Member-only content avilable <i>$desc</i></b>. <br>To see more on this topic, you need a membership. ($expr)" + s"{{/div}}")
          else
            StrAstNode.EMPTY
        } getOrElse {
          StrAstNode.EMPTY
        }
      }
    }
  }

  // to not parse the content, use slines instead of lines
//  def wikiPropITemplate: PS = "{{" ~> opt(".") ~ """template""".r ~ "[: ]".r ~ """[^}]*""".r ~ "}}" ~ slines ~ "{{/" ~ """template""".r ~ "}}" ^^ {
//    case hidden ~ stype ~ _ ~ name ~ _ ~ lines ~ e1 ~ e3 ~ e4 => {
//      val sname = "{{" + hidden.mkString + stype + ":" + name + "}}"
//      RState(sname, lines, e1+e3+e4)
//    }
//  }

  // this is a section inside a template - prevent parsing
  // to not parse the content, use slines instead of lines
  def wikiPropISection: PS = "{{`" ~> """[^}]*""".r <~ "}}" ^^ {
    case whatever => {
      StrAstNode("{{`"+ whatever+ "}}")
    }
  }

  // let it be - avoid replacing it - it's expanded in Wikis where i have the wid
  /**
    * simple value expressions, not a language - these are expanded in some cases
    *
    * like {{$$REALM}} or {{$name}}
    */
  def wikiPropExprS: PS = """\{\{\$\$?""".r ~ """[^}]*""".r <~ "}}" ^^ {
    case kind ~ expr => {
      lazyt { (current, ctx) =>
        StrAstNode(ctx.eval(kind.substring(2), expr))
      }
    }
  }

  // let it be - avoid replacing it - it's expanded in Wikis where i have the wid
  def wikiPropTag: PS = "{{tag" ~ """[: ]""".r ~> """[^}]*""".r <~ "}}" ^^ {
    case name => {
      lazyt { (current, ctx) =>
        val html = Some(Wikis.hrefTag(ctx.we.get.wid, name, name))
        StrAstNode(html.get)
      }.cacheOk
    }
  }

  /** map nvp to html tag attrs */
  private def htmlArgs (args:List[(String,String)]) = args.foldLeft(""){(c, a) => s""" $c ${a._1}="${a._2}" """}

  def wikiPropImg: PS = "{{" ~> "img|photo".r ~ opt("""\.icon|\.small|\.medium|\.large""".r) ~ """[: ]+""".r ~ """[^} ]*""".r ~ optargs <~ "}}" ^^ {
    case skind ~ stype ~ _ ~ name ~ iargs => {
      val width = stype match {
        case Some(".icon") => "width=\"50px\""
        case Some(".small") => "width=\"200px\""
        case Some(".medium") => "width=\"400px\""
        case Some(".large") => "width=\"600px\""
        case _ => ""
      }

      val args = iargs.filter(_._1 != "caption")
      val alt = iargs.toMap.get("caption").filter(_.contains("\"") == false).map(x=>"alt=\""+x+"\"").mkString
      val caption = getCaption(iargs)
      // no alt when contains links
      skind match {
        case "img" =>   StrAstNode(s"""<img src="$name" $width $alt ${htmlArgs(args)} /><br>$caption<br>""")
        case "photo" => StrAstNode(s"""<div style="text-align:center"><a href="$name"><img src="$name" $width $alt ${htmlArgs(args)} ></a></div>$caption\n<br>""")
      }
    }
  }

  private def wikiPropVideo: PS = "{{" ~> ("video" | "slideshow") ~ """[: ]""".r ~ """[^} ]*""".r ~ optargs <~ "}}" ^^ {
    case what ~ _ ~ url ~ args => wpVideo(what, url, args)
  }

  // todo more humane name
  def wikiPropFeedRss: PS = "{{feed.rss" ~ "[: ;]".r ~> """[^ ;}]*""".r <~ "}}" ^^ {
    case xurl => {
      val id = System.currentTimeMillis().toString

      StrAstNode(s"""<div id="$id">""" + Wikis.propLater(id, "/wikie/feed?url="+Enc.toUrl(xurl)) + "</div>")
    }
  }

  def wikiPropCode: PS = "{{" ~> """code""".r ~ "[: ]".r ~ """[^:}]*""".r ~ "}}" ~ opt(CRLF1 | CRLF3 | CRLF2) ~ slines <~ "{{/code}}" ^^ {
    case stype ~ _ ~ name ~ _ ~ crlf ~ lines => {
      TriAstNode(
        s"""<pre><code language="$name">""",
        if(name != "xml" && name != "html") lines else {
          Enc.escapeHtml(lines.s)
        },
        "</code></pre>")
    }
  }

  //=================== XP maps and lists

  private def wikiPropXmap: PS = "{{" ~> """xmap""".r ~ """[: ]""".r ~ """[^}]*""".r ~ "}}" ~ lines <~ ("{{/" ~ """xmap""".r ~ "}}") ^^ {
    case what ~ _ ~ path ~ _ ~ lines => {
      lazyt { (current, ctx) =>
        val html =
          try {
            val s = lines.fold(ctx).s
            ctx.we.map {x =>
              val values = Wikis.irunXp(what, x, path)
              values.map {value=>
                val PAT = """\$\{([^}]+)\}""".r
                PAT replaceSomeIn (s, { m =>
                  if(m.group(1) == "value") Some(value.toString)
                  else None
                })
              }.mkString
            }
          } catch {
            case ex: Throwable => Some("`{{ERROR: "+ex.toString+"}}`")
          }

        StrAstNode(html.mkString)
      }
    }
  }

  private def wikiPropXp: PS = "{{" ~> """xpl?""".r ~ """[: ]""".r ~ """[^}]*""".r <~ "}}" ^^ {
    case what ~ _ ~ path => {
      lazyt { (current, ctx) =>
        StrAstNode( s"""`{{{$what:$path}}}`""", Map())
      }
      // can't expand this during parsing as it will recursively mess up XP
      //      LazyState {(current, we) =>
      //        val html =
      //          try {
      //            we.map(x => Wikis.runXp(what, x, path))
      //          } catch {
      //            case _: Throwable => Some("!?!")
      //          }
      //
      //        SState(html.get)
      //      }
    }
  }

  private def wikiPropQuery: PS = "{{" ~> """tquery""".r ~ """[: ]""".r ~ """[^ :}]*""".r ~ opt("[: ]".r ~> """[^}]*""".r ) <~ "}}" ^^ {
    case what ~ _ ~ path ~ parent => {
      lazyt { (_, ctx) =>
        ctx.we.map{x =>
          val wl = WikiSearch.getList(x.realm, "", parent.mkString, path, 100)

        StrAstNode(
          Wikis.toUl(wl.map(w=>
            Wikis.formatWikiLink(w.realm, w.wid, w.wid.name, w.label, None)._1
          )))
        } getOrElse StrAstNode("?")
      }
    }
  }

  //======================= delimited imports and tables

  def wikiPropCsv: PS = "{{" ~> "r1.delimited:" ~> (wikiPropCsvStart >> { h: CsvHeading => csv(h.delim) ^^ { x => (h, x) } }) <~ "{{/r1.delimited}}" ^^ {
    case (a, body) => {
      val c = body
      //      SState(a.s) + c.filter(_.size > 0).map { l =>
      //        SState("\n* ") + parseW2("[[" + a.what + ":" + l.zip(a.h).filter(c => c._1.length > 0).map {c =>
      //          if ("_" == c._2) c._1 else "{{" + c._2 + " " + c._1 + "}}"
      //        }.mkString(" ") + "]]")
      //      }.reduce(_ + _) + "\n"
      TriAstNode(a.s,
        c.filter(_.size > 0).map { l =>
          TriAstNode(
            "\n* ",
            parseW2("[[" + a.what + ":" + l.zip(a.h).filter(c => c._1.length > 0).map {c =>
              if ("_" == c._2) c._1 else "{{" + c._2 + " " + c._1 + "}}"
            }.mkString(" ") + "]]"),
            "")
        },
        "\n")
    }
  }

  def wikiPropCsv2: PS = "{{" ~> "r1.delimited2:" ~> """[^:]*""".r ~ ":" ~ """[^:]*""".r ~ ":" ~ (wikiPropTableStart >> { h: CsvHeading => csv(h.delim) ^^ { x => (h, x) } }) <~ "{{/r1.delimited}}" ^^ {
    case prefix ~ _ ~ cat ~ _ ~ Tuple2(a, body) => {

      def ecell(cat: String, p: String, a: String, b: String) =
        parseW2("[[" + cat + ":" + p + " " + a + " " + b + "]]")

      TriAstNode(a.s,
        body.map(l =>
          if (l.size > 0) TriAstNode(
            "\n<tr>",
            l.map{c => TriAstNode(
              "<td>" + c + "</td>",
              a.h.tail.map(b => TriAstNode("<td>", ecell(cat, prefix, c, b), "</td>")),
              "")},
            "</tr>")
          else StrAstNode.EMPTY),
        "\n</table>")
    }
  }

  def wikiPropTable: PS = "{{" ~> "r1.table:" ~> (wikiPropTableStart >> { h: CsvHeading => csv(h.delim) ^^ { x => (h, x) } }) <~ "{{/r1.table}}" ^^ {
    case (a, body) => {
      TriAstNode(a.s,
        body.map(l =>
          if (l.size > 0) TriAstNode(
            "\n<tr>",
            l.map(c => TriAstNode("<td>", parseLine(c), "</td>")),
            "</tr>")
          else StrAstNode.EMPTY),
        "\n</table>")
    }
  }

  case class CsvHeading(what: String, s: String, delim: String = ";", h: List[String] = Nil)

  def heading: P = (not("}}") ~> not(",") ~> """.""".r+) ^^ { case l => l.mkString }

  def csvHeadings: Parser[CsvHeading] = heading ~ rep("," ~> heading) ^^ {
    case ol ~ l => CsvHeading("", "", "", List(ol) ::: l)
  }

  def wikiPropCsvStart: Parser[CsvHeading] = """.""".r ~ ":".r ~ """[^:]*""".r ~ opt(":".r ~ csvHeadings) <~ "}}" ^^ {
    case delim ~ _ ~ what ~ head => {
      var s = what + "(s):" + "\n"

      CsvHeading(what, s, delim, head.map(_._2.h).getOrElse(List()))
    }
  }

  /** delim and optional column headings */
  def wikiPropTableStart: Parser[CsvHeading] = """.""".r ~ ":".r ~ opt(csvHeadings) <~ "}}" ^^ {
    case delim ~ _ ~ head => {
      var s = """<table class="table table-striped">"""

      if (head.isDefined)
        s += "\n<thead><tr>" + head.get.h.map(e => "<th>" + e + "</th>").mkString + "</tr></thead>"

      CsvHeading("", s, delim, head.map(_.h).getOrElse(List()))
    }
  }


  //======================= dates
  import razie.tconf.parser.ParserSettings.{mth1, mth2}

  def dates = date1 | date2
  def date1 = """\d\d\d\d""".r ~ "-" ~ """\d\d""".r ~ "-" ~ """\d\d""".r ^^ { case y ~ _ ~ m ~ _ ~ d => "%s-%s-%s".format(y, m, d) }
  def date2 = (mth2 + "|" + mth1).r ~ " *".r ~ """\d[\d]?""".r ~ "[ ,-]*".r ~ """\d\d\d\d""".r ^^ { case m ~ _ ~ d ~ _ ~ y => "%s-%s-%s".format(y, m, d) }
}
