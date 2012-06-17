import org.jibble.pircbot._
import scala.collection.mutable._
import scala.util.Random._
import java.io._

object meh_bot extends App { new meh_bot }

class meh_bot extends PircBot {
    this.setName("_mehbot_")
    this.setVerbose(true)
    this.connect("irc.freenode.net")
    val chan = io.Source.fromFile(".channel").getLines.toList(0)
    this.joinChannel(chan)

    def wordbag = io.Source.fromFile("meh_bot.db").getLines.map(_.trim.toLowerCase).toSet

    override def onMessage(channel:String, sender:String, login:String, hostname:String, message:String) = {
        val newWords = message.replaceAll("[^\\pL'_-]", " ").split(" ").map(_.trim.toLowerCase).filter(_.length > 1).toSet
        val mehFactors = (wordbag & newWords)

        println(mehFactors)
        if(mehFactors.size>1) speak(.2*mehFactors.size)
    }

    def speak(prob:Double=1) = {
        Thread.sleep(1500+nextInt(1000))
        if(nextFloat<=prob) sendMessage(chan, "meh.")
    }
}
