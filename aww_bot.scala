import org.jibble.pircbot._
import scala.collection.mutable._
import scala.util.Random._
import java.io._
import java.net.URL
import de.l3s.boilerpipe.extractors._

object aww_bot extends App { new aww_bot }

class aww_bot extends PircBot {
    this.setName("_awwbot_")
    this.setVerbose(false)
    this.connect("irc.freenode.net")
    val chan = io.Source.fromFile(".channel").getLines.toList(0)
    this.joinChannel(chan)

    def awwbag = io.Source.fromFile("aww_bot.db").getLines.toSet
    val urlReg = """(?i)\b((?:[a-z][\w-]+:(?:/{1,3}|[a-z0-9%])|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'".,<>?«»“”‘’]))""".r
    
    override def onMessage(channel:String, sender:String, login:String, hostname:String, message:String) = {
        val data = urlReg
            .findAllIn(message)
            .toList
            .distinct
            .map(url => {
                var out = "null"
                try {
                    out = KeepEverythingExtractor.INSTANCE.getText(new URL(url))
                } catch { case _ => }
                out
            })
            .reduce(_+_)
            .split("\\s")
            .filter(_.length>2)
            .map(_.toLowerCase)
            .toSet
       
        val awwFactors = (awwbag & data)

        println(data)
        println(awwFactors)
        if(awwFactors.size>1) speak(.20*awwFactors.size)
    }

    def speak(prob:Double=1) = {
        Thread.sleep(nextInt(2000))
        if(nextFloat<=prob) sendMessage(chan, "awww!")
    }
}
