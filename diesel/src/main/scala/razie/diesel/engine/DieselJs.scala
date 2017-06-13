/**
 *  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import razie.diesel.dom.RDOM._
import razie.diesel.dom._

/** wrapper for JS scripts - this is the object `diesel` */
class DieselJs (val ctx:ECtx) {
    def engineId : String = ctx.root.asInstanceOf[DomEngECtx].engine.map(_.id).mkString
    def get (name:String) : String = ctx.apply(name)
    def set (name:String, value:String) : String = {
      ctx.put(P(name, value))
      value
    }

  def msg (msg:String) = "?" // todo implement to add new messages

  def log (msg:String) = razie.Log.log(msg)
}

