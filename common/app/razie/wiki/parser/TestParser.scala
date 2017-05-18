/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki.parser

import razie.wiki.model._

// todo needs to go into tests

object Test /*extends App*/ {
//  import com.tristanhunt.knockoff.DefaultDiscounter._

  val csv1 =
    """
{{delimited:,:Race}}
a,b,c,d
{{/delimited}}
"""

  val csv2 =
    """
{{delimited:,:Race:}}

{{/delimited}}
"""

  val csv3 =
    """
{{delimited:;:Race:name,where,by,when}}
a;b;c;d
{{/delimited}}
"""

  val csv4 =
    """
{{delimited:,:Race:name,where,by,when}}
a,b,c,d
a,,c,d
{{/delimited}}
"""

  //  println(WikiParser.applys(csv1))
  //  println(WikiParser.applys(csv2))
  //  println(WikiParser.applys(csv3))
  //  println(WikiParser.applys(csv4))

  val tabsp = """<table><tr><td>
1



1
</td><td></td></tr></table>"""

  val tabsp1 = """
Ontario (slalom and GS), within the following four age groups:

- Nancy Greene League Racing (ages 7 to 10)
- K1 League Racing (ages 11 and 12),
- K2 League Racing (ages 13 and 14) and
- J Alpine League Racing (ages 15 to 18)
"""
  //  println (WikiParser.applys(tabsp))
  //  println (Wikis.format(WID.NONE, "md", tabsp))
  //  val content = Wikis.preprocess(WID.NONE, "md", tabsp1).s
  val content = """fufu

 """
  //  println(toXHTML(knockoff(content)).toString)
  //  println(Wikis.format (WID("1","2"), "md", content))
  //  println(Wikis.format(WID("1", "2"), "md", content))

  //---------------------

  //  println(f())

  def f() = {
    val ccc = """
{{section:supportreq}}
Support reuested: <p>
<table>
<tr><td>email:</td><td>%s</td></tr>
<tr><td>desc:</td><td>%s</td></tr>
<tr><td>details:</td><td>%s</td></tr>
</table>
<p>
Thank you,<br>The RacerKidz
{{/section}}

"""

    //    val PATT1s = """.*\{\{section:.*\}\}.*\{\{/section\}\}.*""".r
    val PATT1s = """(?s)\{\{section:.*""".r
    val PATT1 = """(?s)\{\{(section):(.*)\}\}(.*)\{\{/(section)\}\}""".r
    val PATT2 = PATT1
    //    val PATT2 = """{{(section|template):([^}]*)}}([^{]*){{/(section|template)}}""".r

    //    PATT1.findAllIn(ccc).toList

  }

  //  Wikis.find(WID("Note", "adfasdf")).foreach { we =>
  //    println(Wikis.format(we.wid, we.markup, we.content, Some(we)))
  //  }

  val r1tabbug = """
{{r1.table:|:POSITION,VOLUNTEER HOURS,FILLED BY}}
1|2|3
a|b|c
{{/r1.table}}
"""
  val xr1tabbug = """
President|10|Karen Whitney
Vice President - Operations|10|Karen Lawson
Past President|10|Position Vacant
Vice-President in Training|10|Mark Booth
Secretary|10|Position Vacant
Treasurer|10|Martin Hambrock
Registrar|10|Ileana Cojocaru
Coaching Administrator|10|John Johnston
Race Administrator|10|Noelle Wood
Hosting Co-ordinator|6 hours per race|Position Vacant
Webmaster|10|Razie Cojocaru
Clothing Co-ordinator|10|Debbie Holmes
Volunteer Co-ordinator|10|Position Vacant
Coaches - paid|10|Varia
Managers|10|Varia
.|[4 hours per week if race/ 1 hr./wk otherwise]|.
Board of Directors|2 hours per meeting - +20|Positions Available
.|.|Secretary for this season and Treasurer for next season (can transition with existing Treasurer this year)
"""

  object nWikiParser extends WikiParserT {
    def realm =  Wikis.RK
  }

  println(nWikiParser.apply(r1tabbug).s)

}
