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
      s"""<a href="/diesel/report$all"><span class="badge" style="background-color: orange" title="Guardian is offline!"><small>... </small></span></a>"""
  }

}
