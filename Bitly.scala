package org.psywerx

class Bitly(apiKey1: String, apiKey2: String) {
  import com.rosaloves.bitlyj._
  import com.rosaloves.bitlyj.Bitly._
  
  lazy val bitly = as(apiKey1, apiKey2)

  def shorten(s: String): Option[String] = {
    try {
      val out = bitly.call(com.rosaloves.bitlyj.Bitly.shorten(s))
      Option(out.getShortUrl)
    } catch {
      case e: Exception =>
        None
    }
  }
  
  // try, or else return original
  def tryShorten(s: String): String = shorten(s).getOrElse(s)
}

