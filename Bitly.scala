package org.psywerx

import com.rosaloves.bitlyj.{Bitly => BitlyAPI}

class Bitly(apiKey1: String, apiKey2: String) {

  lazy val bitlyApi = BitlyAPI.as(apiKey1, apiKey2)

  def shorten(url: String): Option[String] = {
    try {
      Option(bitlyApi.call(BitlyAPI.shorten(url)).getShortUrl)
    } catch {
      case _: Exception =>
        None
    }
  }

  // try, or else return original
  def tryShorten(url: String): String = shorten(url).getOrElse(url)

}

