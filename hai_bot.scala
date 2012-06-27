import org.jibble.pircbot._
import collection.mutable._
import util.Random._
import java.io._
import sys.process._
import scala.concurrent.ops._

object hai_bot extends App { new hai_bot }

class hai_bot extends PircBot {
    val name = "_haibot_2"
    this.setName(name)
    this.setVerbose(true)
    this.connect("irc.freenode.net")
    //val chan = io.Source.fromFile(".channel").getLines.toList(0)
    val chan = "#psywerx-bot"
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
        def sentences = s.split("[.!?]+") // TODO: http://stackoverflow.com/questions/2687012/split-string-into-sentences-based-on-periods
    }
    implicit def String2PimpString(s:String) = new PimpString(s)
    
    val taskFile = "hai_bot/tasks.db"
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

    var lastMsg = ""
    def sendMsg(s:String*):Unit = {
        Thread.sleep(777+nextInt(1500))
        for(i <- 0 until s.size) {
            if((nextFloat<0.5 && lastMsg!=s(i)) || i==s.size-1) {
                lastMsg = s(i)
                sendMessage(chan, s(i))
                return;
            }
        }
    }

    def blocklist = io.Source.fromFile("hai_bot/block.db").getLines.toSet
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
        if(sender.startsWith("Hairy") && nextFloat<0.35) {
            sendMsg("welcome, father.", "welcome back!")
        }
        getTasks(sender)        
    }
                                                                  
    override def onMessage(channel:String, sender:String, login:String, hostname:String, message:String) = {
        val msg = message.toLowerCase
        if(sender.startsWith("Hairy") && message.startsWith("@kill "+name)) {
            sendMsg("bai guise", "bye.", "au revoir!");
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
        } else if(msg.containsAny("0-9", "a-z", "^", "]*") && message.indexOf("[") < message.indexOf("]") && nextFloat<0.75) {
            sendMsg(
                "oooh, is that a regex? I <3 regexes!",
                "Regex! My favorite thing!",
                "mmm, regexes!"
            )
        } else if(msg.startsWithAny("fucking ", "fakin") && msg.sentences(0).split(" ").length < 5 && nextFloat<0.7) {
            sendMsg(
                "does "+msg.sentences(0)+" feel good?",
                "having sex with "+msg.sentences(0).substring(msg.indexOf(" "))+"?"
            )
        } else if(msg.startsWithAny("shut", "fuck") && msg.containsAny("up", "you") && msg.containsAny("haibot", "botko", "bot_") && nextFloat<0.8) {
            sendMsg(
                "Please, don't insult the robot race.",
                """http://bit.ly/yaJI5L""",
                "NO U!",
                "This wouldn't happen if you made us better..."
            )
        } else if(msg.contains("haskell") && nextFloat<0.25) {
            sendMsg("have you tried using monads?")
        } else if(msg.contains(" vim ") && nextFloat<0.25) {
            sendMsg("use pathogen.")
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

