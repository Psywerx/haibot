import org.jibble.pircbot._
import scala.collection.mutable._
import scala.util.Random._
import java.io._

object rnd_bot extends App { new rnd_bot }

class rnd_bot extends PircBot {
    this.setName("_rndbot_")
    this.setVerbose(true)
    this.connect("irc.freenode.net")
    val chan = io.Source.fromFile(".channel").getLines.toList(0)
    this.joinChannel(chan)

    concurrent.future(while(true) {
        Thread.sleep(35000+nextInt(1500000))
        speak(.35)
    })


    val wordbag1 = ListBuffer[String]()
    val wordbag2 = io.Source.fromFile("wordbag.db").getLines.toList

    var last = false
    speak()

    override def onMessage(channel:String, sender:String, login:String, hostname:String, message:String) = {
        last = false
        val newWords = message.replaceAll("[^\\pL,']", " ").split(" ").filter(_.length() > 1)
        wordbag1 ++= newWords
        
        if(message.contains(getNick)) speak(.9) else speak(.075)
    }

    def speak(prob:Double=1) = {
        Thread.sleep(1000+nextInt(3700))
        if(nextFloat<=prob && !last) {
            var message = shuffle(if(wordbag1.length>1500 && nextFloat<0.7) wordbag1 else (wordbag1++wordbag2)).take(nextInt(5)+7).mkString(" ")+(if(nextFloat<0.4) "." else "")
            message = message.head + message.tail.toLowerCase
            sendMessage(chan, message)
            last = true;
        }
    }
}

