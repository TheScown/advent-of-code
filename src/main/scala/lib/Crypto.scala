package space.scown.adventofcode
package lib

import java.security.MessageDigest

object Crypto {

  private lazy val md5 = MessageDigest
    .getInstance("MD5")

  def md5(s: String): String = {
    md5.digest(s.getBytes("UTF-8"))
      .map("%02x".format(_))
      .mkString("")
  }

}
