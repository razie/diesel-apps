/**
 *   ____    __    ____  ____  ____,,___     ____  __  __  ____
 *  (  _ \  /__\  (_   )(_  _)( ___)/ __)   (  _ \(  )(  )(  _ \           Read
 *   )   / /(__)\  / /_  _)(_  )__) \__ \    )___/ )(__)(  ) _ <     README.txt
 *  (_)\_)(__)(__)(____)(____)(____)(___/   (__)  (______)(____/    LICENSE.txt
 */
package razie.wiki

import java.net.URLDecoder
import java.net.URLEncoder
import java.security.MessageDigest

/**
 * simple encryption service - used to encrypt all sorts of things like emails whatnot
 *
 * provide your own implementation DES with a per app configured key should do fine
 */
trait EncryptService {
  def enc(s: String): String
  def dec(s: String): String
}

/** sample stub clear text - no encryption */
object NoEncryptService extends EncryptService {
  def enc(s: String): String = s
  def dec(s: String): String = s
}

/** set this in your Global::beforeStart()
  * todo inject instead
  */
object EncryptService {
  var impl: EncryptService = NoEncryptService
}

/** encription utilities */
object Sec {

  // rich String with enc/dec
  implicit class EncryptedS(s: String) {
    def enc = EncryptService.impl.enc(s)
    def dec = EncryptService.impl.dec(s)
    def encBase64 = Base64 enc s
    def decBase64 = Base64 dec s
    def encUrl = Enc toUrl s
    def decUrl = Enc fromUrl s
  }
}

/** base 64 utility */
object Base64 {
  import org.apache.commons.codec.binary.Base64
  def enc(s: String) = new Base64(true).encode(s)
  def dec(s: String) = new Base64(true).decode(s)
}

/** utility to encrypt/decrypt stuff*/
object Enc {
  def apply(orig: String) = EncryptService.impl.enc(orig)
  def unapply(encoded: String): Option[String] = Some(EncryptService.impl.dec(encoded))

  /** url encode */
  def toUrl(orig: String) = URLEncoder.encode(orig, "UTF8")
  /** url decode */
  def fromUrl(orig: String) = URLDecoder.decode(orig, "UTF8")

  // TODO remove this play 2.0 workaround
  def toSession(orig: String) = orig.replaceAll("-", "RAZIEDASH")
  def fromSession(orig: String) = orig.replaceAll("RAZIEDASH", "-")

  /** hash a string - result must be safe BASE64 asciis */
  def hash(input: String): String = {
    md() update input.getBytes()
    val arr = md().digest
    new org.apache.commons.codec.binary.Base64(true).encodeToString(arr)
  }

  // I hope this initializes later, when needed...heh, that's pray programming, damn it!
  private object md {
    val i = MessageDigest.getInstance("SHA")
    def apply() = { i }
  }

  // escape html characters
  def escapeHtml (s:String) = s.replaceAll("<", "&lt;").replaceAll(">", "&gt;").replaceAll("\"", "&quot;")
  def unescapeHtml (s:String) = s.replaceAll("&lt;", "<").replaceAll("&gt;", ">").replaceAll("&quot;", "\"")
}

object EncUrl {
  def apply(orig: String) = URLEncoder.encode(Enc(orig), "UTF8")
  def unapply(encoded: String): Option[String] = Some(URLDecoder.decode(encoded, "UTF8"))
}
