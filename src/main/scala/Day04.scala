import java.security.MessageDigest

import scala.io.Source

object Day04 extends App {

  val key = Source.fromInputStream(getClass.getResourceAsStream("Day04.input")).mkString.getBytes
  var stream = Stream.from(0)

  val md5 = MessageDigest.getInstance("MD5")

  def allZero(arr: Array[Byte], index: Int): Boolean = {
    val mask = if (index % 2 == 0) 0xF0 else 0x0F

    val res = (arr(index / 2) & mask) == 0

    if (index == 0) res else res && allZero(arr, index - 1)
  }

  def findNumber(pattern: String): Int = {
    for (i <- stream) {
      md5.update(key)
      val hashed = md5.digest(i.toString.getBytes) //.map("%02X".format(_)).mkString

      if (allZero(hashed, pattern.length - 1)) {
        return i
      }
      /*
      if (hashed.substring(0, 5) == pattern) {
        return i
      }*/
    }
    0
  }

  println(findNumber("00000"))
  println(findNumber("000000"))
}
