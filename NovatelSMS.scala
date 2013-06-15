package org.psywerx

object NovatelSMS {  
  val responseMap = Map(
    "ok" -> "Ok",
    "auth" -> "Unsuccessful authentication",
    "to_number" -> "Invalid destination number",
    "no_msg" -> "Missing SMS text",
    "assert" -> "Assertion failed"
  ) withDefaultValue "Unknown error"
}

class NovatelSMS(username: String, password: String) {
  def sendSMS(msg: String, number: String): String = {
    try {
      //TODO: base64 encode everything, as is recommended by API
      assert(msg.length <= 500)
      assert(number matches "(00)?[386][0-9]+")
      assert(!(username contains "&"))
      assert(!(password contains "&"))
      
      val msg64 = new sun.misc.BASE64Encoder().encode(msg.getBytes)
      
      val response = io.Source.fromURL(s"""http://smsgw0.novatel.si/sms.php?username=${username}&password=${password}&to=${number}&msg_enc_type=base64&msg=$msg64""").mkString.split(" ")
      
      response(0) match {
        case "ok" => NovatelSMS.responseMap("ok")
        case "error" if(response.size == 2) => NovatelSMS.responseMap(response(1))
        case _ => NovatelSMS.responseMap("error")
      }
    } catch {
      case e: AssertionError=>
        println(e)
        NovatelSMS.responseMap("assert")
      case e: Exception =>
        println(e)
        NovatelSMS.responseMap("error")
    }
  }
}

