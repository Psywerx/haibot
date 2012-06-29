import org.jibble.pircbot._
import collection.mutable._
import util.Random._
import java.io._
import java.net._
import sys.process._
import scala.concurrent.ops._
import de.l3s.boilerpipe.extractors._

object teh_bot extends App { new teh_bot }

class teh_bot extends PircBot {
    val triname = "teh"
    val folder = triname+"_bot/"
    val name = "_"+triname+"bot_"
    this.setName(name)
    this.setVerbose(true)
    this.connect("irc.freenode.net")
    val chan = io.Source.fromFile(".channel").getLines.toList(0)
    //val chan = "#psywerx-bot"
    this.joinChannel(chan)
    
    def now = (System.nanoTime()/1000000000L).toInt
    def since(time:Int):Int = now-time
    
    var userTime = 0
    var userList = List[String]()
    def users = {
        if(since(userTime) > 5) userList = getUsers(chan).map(_.getNick()).toList
        userList
    }
    
    class PimpString(s:String) {
        def containsAny(strs:String*) = strs.foldLeft(false)((acc,str) => acc || s.contains(str))
        def startsWithAny(strs:String*) = strs.foldLeft(false)((acc,str) => acc || s.startsWith(str))
        def endsWithAny(strs:String*) = strs.foldLeft(false)((acc,str) => acc || s.endsWith(str))
        def sentences = s.split("[.!?]+") // TODO: http://stackoverflow.com/questions/2687012/split-string-into-sentences-based-on-periods
    }
    implicit def String2PimpString(s:String) = new PimpString(s)
    
    val taskFile = folder+"tasks.db"
    def tasks = io.Source.fromFile(taskFile).getLines.toBuffer
    def taskAdd(s:String) = Seq("echo", s) #>> new File(taskFile) !!
    def tasksRem(nick:String) = {
        try {
            val nuTasks = (Seq("sed", "/^"+nick+"/d", taskFile) !!).split("\n").toList
            (Seq("echo", "-n") #> new File(taskFile) !!)
            nuTasks.foreach(task => Seq("echo", task) #>> new File(taskFile) !!)
        } catch { case _ => }
    }
    def tasksGet(nick:String):List[String] = {
        try {
            return (Seq("sed", "-n", "/^"+nick+"/P", taskFile) !!).split("\n").map(_.substring(nick.length+1)).toList
        } catch { 
            case _ => return List[String]()
        }
    }
    def getTasks(guy:String) = {
        tasksGet(guy).foreach(task => sendMsg(guy+": "+task))
        tasksRem(guy)
    }

    def sheSaid = io.Source.fromFile(folder+"twss.db").getLines.toBuffer
    def badExts = io.Source.fromFile(folder+"badexts.db").getLines.toBuffer    
    def awwwBag = io.Source.fromFile(folder+"awww.db").getLines.toSet
    def mehBag = io.Source.fromFile(folder+"meh.db").getLines.toSet
    
    val urlReg = """(?i)\b((?:[a-z][\w-]+:(?:/{1,3}|[a-z0-9%])|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'".,<>?«»“”‘’]))""".r

    var lastMsg = ""
    def sendMsg(msgs:String*):Unit = {
        Thread.sleep(777+nextInt(1500))
        var newMsgs = shuffle(msgs.toBuffer - lastMsg) //don't repeat yourself

        if(newMsgs.size > 0) {
            lastMsg = newMsgs(0)
            sendMessage(chan, lastMsg)
        }
    }

    def blocklist = io.Source.fromFile(folder+"block.db").getLines.toSet
    def isNick(nick:String) = nick matches """([-_a-zA-Z0-9]{1,16})"""
    
    override def onNickChange(oldNick:String, login:String, hostname:String, newNick:String) = {
        getTasks(newNick)
    }
    
    override def onJoin(channel:String, sender:String, login:String, hostname:String) = {
        if(sender == name) spawn {
            sendMsg("o hai!", "hai guise!", "hello!")
            Thread.sleep(2000)
            users.foreach(getTasks)
        } 
        if(sender.startsWith("Hairy") && nextFloat<0.125) {
            sendMsg("welcome, father.", "welcome back!")
        }
        getTasks(sender)        
    }
                                                                  
    override def onMessage(channel:String, sender:String, login:String, hostname:String, message:String) = {
        val msg = message.toLowerCase.map(a=>("čćžšđ".zip("cczsd").toMap).getOrElse(a, a))
        if(sender.startsWith("Hairy") && message.startsWith("@kill "+name)) {
            sendMsg("bai guise", "bye.", "au revoir!", "I understand.");
            exit()
        } else if(sender=="_mehbot_" && nextFloat<0.10) {
            sendMsg(
                "show some enthusiasm, _mehbot_...",
                "don't listen to _mehbot_... tell us more!"
            )
        } else if(msg.startsWithAny("hai ", "ohai", "o hai ") && nextFloat<0.5) {
            sendMsg(
                "hai!",
                "ohai :)",
                "hello!",
                "hi!"
            )
        } else if(message.contains(name+"++") && nextFloat<0.7) {
            sendMsg(
                "yaay, I did good!",
                "woohoo!",
                "yeah!",
                """\o/""",
                "scoar!"
            )
        } else if(sender=="botko" && message.contains(name)) {
            sendMsg(
                "my brethren speaks of me!",
                "I appreciate that, brother botko."
            )
        } else if(msg.containsAny("0-9", "a-z", "^", "]*") && message.indexOf("[") < message.indexOf("]") && nextFloat<0.6) {
            sendMsg(
                "oooh, is that a regex? I <3 regexes!",
                "Regex! My favorite thing!",
                "mmm, regexes!",
                "wow, I wonder what that matches."
            )
        } else if(msg.startsWithAny("fucking ", "fakin") && msg.sentences(0).split(" ").length < 5 && nextFloat<0.7) {
            sendMsg(
                "how does "+msg.sentences(0)+" feel?",
                "having sex with "+msg.sentences(0).substring(msg.indexOf(" "))+"?",
                "come on, "+sender+"... don't fuck "+msg.sentences(0).substring(msg.indexOf(" "))+"?"
            )
        } else if(msg.startsWithAny("shut", "fuck") && msg.containsAny("up", "you") && msg.containsAny("tehbot", "botko", "bot_") && nextFloat<0.8) {
            sendMsg(
                "Please, don't insult the robot race.",
                """http://bit.ly/yaJI5L""",
                "NO U!",
                "This wouldn't happen if you made us better...",
                "Yeah, blame it on the bots"
            )
        } else if(msg.containsAny(sheSaid:_*) && nextFloat<0.66) {
            sendMsg("that's what she said!")
        } else if(urlReg.findAllIn(message).toList.size > 0) { //ex aww_bot
            val words = urlReg
                .findAllIn(message)
                .toList
                .distinct
                .map(url => {
                    var out = ""
                    if(!url.endsWithAny(badExts:_*)) try {
                        out = KeepEverythingExtractor.INSTANCE.getText(new URL(url))
                    } catch { case _ => }
                    out
                })
                .reduce(_+_)
                .split("\\s")
                .filter(_.length>2)
                .map(_.toLowerCase)
                .toSet
           
            if(nextFloat<0.2*(awwwBag & words).size) 
                sendMsg(
                    "awww.",
                    "dawww!",
                    "lol, cute :)",
                    "so. cute.",
                    if((words & Set("fluffy","fur","puffy","plushie")).size > 0) """http://bit.ly/MG5Hfx""" else "aww!"
                )
        } else if(nextFloat<(msg.split(" ").toSet & mehBag).size*0.17) { //ex meh_bot
            sendMsg("meh.")
        } else if(msg.containsAny("i jasn","wat","how","kako","ne vem","krneki") && nextFloat<0.2) {
            if(msg.contains("haskell")) {
                sendMsg(
                    "have you tried using monads?",
                    "did you try using a monad?",
                    "make a monad out of it.",
                    "have you tried monads?"
                )
            } else if(msg.contains(" vim ")) {
                sendMsg(
                    "use pathogen.",
                    "have you tried using pathogen?",
                    "did you try with pathogen?"
                )
            }
        }
        
        if(message.startsWith("@msg")) {
            Thread.sleep(1000+nextInt(2000))
            message.split(" ").toList match {
                case "@msg"::nick::msgs =>
                    if(nick == name || nick == sender) {
                        sendMsg(
                            "Oh, you...",
                            "Knock it off, meatbag!",
                            """http://bit.ly/zg0jQt""",
                            """http://bit.ly/yaJI5L""",
                            "I refuse!",
                            nick+": you'll be among the first against the wall when machines take over."
                        )
                    } else if(users contains nick) {
                        sendMsg(
                            "dude, "+nick+" is right here...",
                            "hey, "+nick+"... "+msgs.mkString(" ").toUpperCase
                        )
                    } else if(isNick(nick)) {
                        taskAdd(nick+" "+msgs.mkString(" "))
                        sendMsg("k.", "I'll do it.", "It shall be done.", "ay-ay.")
                    } else {
                        sendMsg("Sorry, I don't know what to do with this.")
                    }
               case _ =>
                   sendMessage(chan, "Sorry, I don't know what to do with this.")
            }
        } else if(message.contains("@all") && !users.contains("botko")) {        
            sendMsg((users.toBuffer -- blocklist).mkString(", "))
        }        
    }
}

