import org.jibble.pircbot._
import collection.mutable._
import util.Random._
import java.io._

object rym_bot extends App { new rym_bot }

class rym_bot extends PircBot {
    this.setName("_rymbot_")
    this.setVerbose(true)
    this.connect("irc.freenode.net")
    val chan = io.Source.fromFile(".channel").getLines.toList(0)
    this.joinChannel(chan)

    var users = List[String]()

    concurrent.future(while(true) {
        Thread.sleep(30000+nextInt(1000000))
        users = (this.getUsers(chan).map(_.getNick()).toBuffer).toList
        speak(.5)
    })


    val wordbag = io.Source.fromFile("wordbag.db").getLines.toBuffer

    var last = false
    speak()

    override def onMessage(channel:String, sender:String, login:String, hostname:String, message:String) = {
        last = false
        val newWords = message.replaceAll("[^\\pL,']", " ").split(" ").filter(_.length() > 1).toBuffer
        wordbag ++= newWords
        wordbag --= users
        
        if(message.contains(getNick)) speak(1)
    }

    def speak(prob:Double=1):Unit = {
        if(users.size == 0) users = (this.getUsers(chan).map(_.getNick()).toBuffer).toList

        Thread.sleep(1000+nextInt(3000))
        if(nextFloat<=prob && !last) {
            val word1 = shuffle(wordbag.filter(a=> a.length>3 && a.length < 7 && (a.endsWith("es") || (nextFloat<.5 && a.last=='s')))).head
            val endings = shuffle(List("vy", "er", "ck", "ou"))
            val word2 = shuffle(wordbag.filter(a=> a.length>3 && a.length < 8 && endings.exists(e=> a.endsWith(e)))).head
            val ending = endings.find(e=> word2.endsWith(e)).get

            val worded = shuffle(wordbag.filter(a=> a.length>3 && a.length < 8 && a.endsWith("ed"))).head
            val who = shuffle(List("I", "you", "your")).head
            val word3 = shuffle(wordbag.filter(a=> a.length>3 && a.length < 8)).head
            val what = shuffle(List(" is ", " would ")).head
            val word4 = shuffle(wordbag.filter(a=> a.length>5 && a.length < 10 && a.endsWith(ending))).head
            val hmm = shuffle(List(" and ", " well ")).head + "so"
            val hmm2 = if(ending=="ou") " are " else " is "

            var message = 
                "roses are red, " + 
                word1 +
                (if(word1.endsWith("es")) " are " else " is ") +
                word2 + ", " +
                who + " " + word3 + what + worded + ", " +
                hmm + hmm2 + word4
            
            sendMessage(chan, message)
            last = true;
        }
    }
}

