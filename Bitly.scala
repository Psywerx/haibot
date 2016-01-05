package org.psywerx

class Bitly(apiKey1: String, apiKey2: String) {
  import com.rosaloves.bitlyj.Bitly

  lazy val bitly = Bitly.as(apiKey1, apiKey2)

  def shorten(s: String): Option[String] = {
    try {
      Option(bitly.call(Bitly.shorten(s)).getShortUrl)
    } catch {
      case e: Exception =>
        None
    }
  }

  // try, or else return original
  def tryShorten(s: String): String = shorten(s) getOrElse s
}

