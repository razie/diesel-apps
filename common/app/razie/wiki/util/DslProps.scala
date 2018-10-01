package razie.wiki.util

import razie.wiki.WikiConfig
import razie.wiki.model._

/**
 * dsl processing - entries with a props section
  *
  * @param section comma separated list of sections
 */
class DslProps (val we:Option[WikiPage], section:String, extra:Seq[(String,String)] = Seq()) {
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

  override def toString = "AS seq: " + propSeq.mkString + "\nAs map: " + props.mkString

  // todo avoid reload, swap the entire class and use VAL instead of DEF in Website - stop parsing stuff all the time
  def reload(we:WikiPage): Unit = {
    ipropSeq = extra ++
      section.split(",").toSeq.flatMap( sec=>
        we.section("section", sec).toArray flatMap (ws=>WikiConfig.parsep(ws.content))
    )
    iprops = ipropSeq.toMap[String,String]
  }

  def :: (other:DslProps) : DslProps =
    new DslProps (we, section, other.ipropSeq)
}

