package razie.wiki.util

import play.api.mvc.Request
import razie.OR._
import razie.wiki.WikiConfig
import razie.wiki.model._

/**
 * dsl processing - entries with a props section
 */
class DslProps (val we:Option[WikiEntry], section:String, extra:Seq[(String,String)] = Seq()) {
  private var ipropSeq = extra
  private var iprops = ipropSeq.toMap[String,String]

  //todo stupid init pattern
  lazy val init = {
    we.map(reload)
  }

  def propSeq = {init; ipropSeq}
  def props = {init; iprops}

  def prop (s:String) = props get s
  def wprop (s:String) = (this prop s).flatMap(x=>WID.fromPath(x))
  def bprop (s:String) = (this prop s).map(_.toUpperCase).map {p=>
    p == "TRUE" || p == "YES" || p == "1" || p == "ON"
  }

  override def toString = propSeq.mkString

  def reload(we:WikiEntry): Unit = {
    ipropSeq = extra ++ (we.section("section", section).toArray flatMap (ws=>WikiConfig.parsep(ws.content)))
    iprops = ipropSeq.toMap[String,String]
  }
}

