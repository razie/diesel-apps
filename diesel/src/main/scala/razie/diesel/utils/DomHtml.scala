/**
  *  ____    __    ____  ____  ____,,___     ____  __  __  ____
  * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
  *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
  * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
  */
package razie.diesel.utils

/** DOM Html helpers
  */
object DomHtml {

  def quickBadge(failed:Int, total:Int, duration:Long, all:String="") = {
    if (failed > 0)
      s"""<a href="/diesel/report$all"><span class="badge" style="background-color: red" title="Guardian: tests failed ($duration msec)">$failed / $total </span></a>"""
    else if(total > 0)
      s"""<a href="/diesel/report$all"><span class="badge" style="background-color: green" title="Guardian: all tests passed ($duration msec)">$total </span></a>"""
    else
      s"""<a href="/diesel/report$all"><span class="badge" style="background-color: orange" title="No tests run!"><small>... </small></span></a>"""
  }

}
