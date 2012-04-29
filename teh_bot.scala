import org.jibble.pircbot._
import scala.collection.mutable._
import scala.util.Random._
import java.io._

object teh_bot extends App {
    new teh_bot
}

class teh_bot extends PircBot {
    this.setName("teh_bot")
    this.setVerbose(true)
    this.connect("irc.freenode.net")
    this.joinChannel("#psywerx")

    concurrent.future(while(true) {
        Thread.sleep(35000+nextInt(1000000))
        speak(.4)
        println("spammybot")
    })


    val wordbag1 = ListBuffer[String]()
    val wordbag2 = io.Source.fromFile("wordbag.db").getLines.toList

    //val wb = new PrintWriter(new File("wordbag.db"))
    var last = false
    speak()

    override def onMessage(channel:String, sender:String, login:String, hostname:String, message:String) = {
        last = false
        val newWords = message.replaceAll("[^\\pL,']", " ").split(" ").filter(_.length() > 1)
        //newWords.foreach(wb.println)
        wordbag1 ++= newWords
        
        if(message.contains("teh_bot")) speak(.9) else speak(.1)
    }

    def speak(prob:Double=1) = {
        Thread.sleep(1000+nextInt(3700))
        if(nextFloat<=prob && !last) {
            var message = shuffle(if(wordbag1.length>1500 && nextFloat<0.7) wordbag1 else (wordbag1++wordbag2)).take(nextInt(5)+7).mkString(" ")+(if(nextFloat<0.4) "." else "")
            message = message.head + message.tail.toLowerCase
            sendMessage("#psywerx", message)
            last = true;
        }
    }
}

