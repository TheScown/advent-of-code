package space.scown.adventofcode
package lib

import org.apache.commons.codec.binary.Hex

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

object Crypto {

  private lazy val md5 = MessageDigest
    .getInstance("MD5")

  def md5(s: String): String = {
    new String(Hex.encodeHex(md5.digest(s.getBytes(StandardCharsets.UTF_8))))
  }

}
