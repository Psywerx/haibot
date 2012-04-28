import org.jibble.pircbot._
import scala.collection.mutable._
import scala.util.Random._

object teh_bot extends App {
    new teh_bot
}

class teh_bot extends PircBot {
    this.setName("teh_bot")
    this.setVerbose(true)
    this.connect("irc.freenode.net")
    this.joinChannel("#psywerx")

    val wordbag = ListBuffer[String]()

    override def onMessage(channel:String, sender:String, login:String, hostname:String, message:String) = {
        wordbag ++= message.replace("[^\\pL]", " ").split(" ").filter(_.length() > 1)
        if(nextFloat>0.75) sendMessage("#psywerx", shuffle(wordbag).take(nextInt(5)+5).mkString(" "))
    }
}
