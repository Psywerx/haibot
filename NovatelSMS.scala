package org.psywerx

import java.net.{ URLEncoder, MalformedURLException }
import sun.misc.BASE64Encoder

object NovatelSMS {  
  val responseMap = Map(
    "ok" -> "Ok",
    "auth" -> "Unsuccessful authentication",
    "to_number" -> "Invalid destination number",
    "no_msg" -> "Missing SMS text",
    "assert" -> "Assertion failed",
    "url" -> "Malformed URL",
    "" -> "General error"
  ) withDefaultValue "Unknown error"
}

class NovatelSMS(username: String, password: String) {  
  private val BASE64Encoder = new BASE64Encoder
  def sendSMS(msg: String, number: String): String = {
    try {
      //TODO: base64 encode everything, as is recommended by API
      assert(msg.length <= 500)
      assert(number matches "(00)?[386][0-9]+")
      assert(!(username contains "&"))
      assert(!(password contains "&"))
      
      val msg64 = URLEncoder.encode(BASE64Encoder.encode(msg.getBytes))
      
      val url = s"""http://smsgw0.novatel.si/sms.php?msg_enc_type=base64&msg=${msg64}&username=${username}&password=${password}&to=${number}"""
      println("Sending SMS as: "+url)
      
      val response = io.Source.fromURL(url).mkString.split(" ")
      println("SMS response: "+response.mkString(" "))
      
      response(0) match {
        case "ok" => NovatelSMS.responseMap("ok")
        case "error" if(response.size == 2) => NovatelSMS.responseMap(response(1))
        case _ => NovatelSMS.responseMap("error")
      }
    } catch {
      case e: AssertionError =>
        println(e)
        NovatelSMS.responseMap("assert")
      case e: MalformedURLException =>
        println(e)
        NovatelSMS.responseMap("url")
      case e: Exception =>
        println(e)
        NovatelSMS.responseMap("error")
    }
  }
}

