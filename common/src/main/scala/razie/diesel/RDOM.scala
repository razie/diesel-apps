package razie.diesel

import razie.wiki.dom.WikiDomain

/** simple, neutral domain model representation */
object RDOM {
  // archtetypes
  val ARCH_SPEC = "Spec"
  val ARCH_ROLE = "Role"
  val ARCH_ENTITY = "Entity"
  val ARCH_MI = "MI"

  def smap(s:String) (f:String=>String) =  if(s != null && s.length > 0) f(s) else ""
  def mks(l:List[_], pre:String, sep:String, post:String="", indent:String="") = if(l.size>0) pre + l.map(indent + _).mkString(sep) + post else ""

  class CM // abstract Class Member

  /** represents a Class */
  case class C (name:String, archetype:String, stereotypes:String, base:List[String], typeParam:String, parms:List[P]=Nil, methods:List[F]=Nil, assocs:List[A]=Nil) {
    override def toString = s"""class <b><a href="/wikie/show/Category:$name">$name</a></b> """ +
      smap(typeParam) (" [" + _ + "]") +
      smap(archetype) (" &lt;" + _ + "&gt;") +
      smap(stereotypes) (" &lt;" + _ + "&gt;") +
      (if(base.exists(_.size>0)) "extends " else "") + base.map("<b>" + _ + "</b>").mkString +
      mks(parms, " (", ", ", ") ") +
      mks(methods, "{<br>", "<br>", "<br>}", "&nbsp;&nbsp;")

    def fullHtml = s"""class <b><a href="/wikie/show/Category:$name">$name</a></b> """ +
      smap(typeParam) (" [" + _ + "]") +
      smap(archetype) (" &lt;" + _ + "&gt;") +
      smap(stereotypes) (" &lt;" + _ + "&gt;") +
      (if(base.exists(_.size>0)) "extends " else "") + base.map("<b>" + _ + "</b>").mkString +
      mks(parms, " (<br>", ",<br> ", ") ", "&nbsp;&nbsp;") +
      mks(methods, "{<br><hr>", "<br>", "<br><hr>}", "&nbsp;&nbsp;")
  }

  /** represents a parameter/member/attribute */
  case class P (name:String, ttype:String, ref:String, multi:String, dflt:String, expr:String = "") extends CM {
    override def toString =
      s"<b>$name</b>" +
      smap(ttype) (":" + ref + _) +
      smap(multi)(identity) +
      smap(dflt) ("=" + _)
  }

  /** a function / method */
  case class F (name:String, parms:List[P], ttype:String, body:List[EXEC]=List.empty) extends CM {
    override def toString = s"  def <b>$name</b> " +
      mks(parms, " (", ", ", ") ") +
      smap(ttype) (":" + _)
  }

  /** an executable statement */
  trait EXEC {
    def sForm:String
    def exec(ctx:Any, parms:Any*):Any

    override def toString = sForm
  }

  case class V (name:String, value:String)  // attr value

  case class O (name:String, base:String, parms:List[V]) { // object = instance of class
    def toJson = parms.map{p=> p.name -> p.value}.toMap
  }

  class D  (val roles:List[(String, String)], val ac:Option[AC]=None) //diamond association

  /** name is not required */
  case class A  (name:String, a:String, z:String, aRole:String, zRole:String, parms:List[P]=Nil, override val ac:Option[AC]=None) //association
    extends D (List(a->aRole, z->zRole), ac) {
    override def toString = s"assoc $name $a:$aRole -> $z:$zRole " +
      mks(parms, " (", ", ", ") ")
  }
  case class AC (name:String, a:String, z:String, aRole:String, zRole:String, cls:C) //association class

  // Diamond Class

  case class E (name:String, parms:List[P], methods:List[F]) //event
  case class R (name:String, parms:List[P], body:String) //rule
  case class X (body:String) //expression
  case class T (name:String, parms:List[P], body:String) //pattern

}


