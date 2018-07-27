/**
 *  ____    __    ____  ____  ____,,___     ____  __  __  ____
 * (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *  )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 * (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.diesel.engine

import jdk.nashorn.internal.runtime.Undefined
import razie.diesel.dom.RDOM._
import razie.diesel.dom._

/** wrapper for JS scripts - this is the object `diesel` */
class DieselJs (val ctx:ECtx) {
    def engineId : String = ctx.root.engine.map(_.id).mkString

    def get (name:String) : Any = {
      val p = ctx.getp(name)

      p match {
        case Some(x) if x.ttype == "JSON" => x.dflt // todo json type to object
        case Some(x) => x.dflt
        case None => Undefined.getUndefined // should I ?
      }
    }

    def set (name:String, value:String) : String = {
      ctx.put(P(name, value))
      value
    }

  def msg (msg:String) = "?" // todo implement to add new messages

  def log (msg:String) = razie.Log.log(msg)
}

