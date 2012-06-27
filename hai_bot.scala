import org.jibble.pircbot._
import collection.mutable._
import util.Random._
import java.io._
import sys.process._

object hai_bot extends App { new hai_bot }

class hai_bot extends PircBot {
    val name = "_haibot_"
    this.setName(name)
    this.setVerbose(true)
    this.connect("irc.freenode.net")
    val chan = io.Source.fromFile(".channel").getLines.toList(0)
    //val chan = "#psywerx-bots"
    this.joinChannel(chan)
    
    def blocklist = io.Source.fromFile("hai_bot.block.db").getLines.toSet
    def isNick(nick:String) = nick matches """([-_a-zA-Z0-9]{1,16})"""
    def users = {
        Thread.sleep(500+nextInt(250))
        shuffle(this.getUsers(chan).map(_.getNick()).toBuffer)
    }
    
    val taskFile = "hai_bot/hai_bot.tasks.db"
    def tasks = io.Source.fromFile(taskFile).getLines.toBuffer
    def taskAdd(s:String) = Seq("echo", s) #>> new File(taskFile) !!
    def tasksRem(nick:String) = {
        try {
            val nuTasks = (Seq("sed", "/^"+nick+"/d", taskFile) !!).split("\n").toList
            (Seq("echo", "") #> new File(taskFile) !!)
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

    def sendMsg(s:String) = {
        Thread.sleep(777+nextInt(2000))
        sendMessage(chan, s)
    }


    def getTasks(guy:String) = {
        tasksGet(guy).foreach(task => sendMsg(guy+": "+task))
        tasksRem(guy)
    }
    
    
    override def onNickChange(oldNick:String, login:String, hostname:String, newNick:String) = {
        getTasks(newNick)
    }
    
    override def onJoin(channel:String, sender:String, login:String, hostname:String) = {
        if(sender == name) {
            sendMsg("o hai!")
            concurrent.ops.future {
                Thread.sleep(2500)
                users.foreach(user=> {
                    println(user)
                    getTasks(user)
                })
            }
        } else if(sender.startsWith("Hairy")) {
          if(nextFloat<0.35) 
            sendMsg("welcome, father.")
          else if(nextFloat<0.25) 
            sendMsg("ohai!")
        } else {
            getTasks(sender)        
        }
    }
                                                                  
    override def onMessage(channel:String, sender:String, login:String, hostname:String, message:String) = {
        if(sender=="_mehbot_" && nextFloat<0.17) {
          if(nextFloat<0.35) 
            sendMsg("show some enthusiasm, brother.")
          else if(nextFloat<0.82) 
            sendMsg("I think it's kind of cool... tell us more.")            
        } else if(message.contains("ohai ") && nextFloat<0.25) {
            sendMsg("hai!")
        } else if(message.contains(name+"++") && nextFloat<0.85) {
            sendMsg("yaay, I did good!")
        } else if(sender == "botko" && message.contains(name)) {
            sendMsg("my brethren speaks with/of me.")
        } else if((message.contains("0-9") || message.contains("a-z") || message.contains("A-Z") || message.contains("^")) 
          && message.indexOf("[") < message.indexOf("]") && nextFloat<0.85) {
          if(nextFloat<0.5) 
            sendMsg("oooh, is that a regex? I <3 regexes!")
          else
            sendMsg("Regex!")
        } else if(message.startsWith("fucking") && message.split(" ").length < 5) {
          if(nextFloat<0.4)
            sendMsg("does "+message.replaceAll("[.!?]","")+" feel good?")
          else if(nextFloat<0.75) 
            sendMsg("having sex with "+message.split(" ").tail+"?")
        } else if((message.startsWith("shut") || message.startsWith("fuck"))
          && (message.contains("up") || message.contains("you"))
          && (message.contains("haibot") || message.contains("botko") || message.contains("bot_"))) {
          if(nextFloat<0.45)
            sendMsg("NO U!")
          else if(nextFloat<0.55) 
            sendMsg("Putting robots down makes you feel like a big man, huh?")
        }
        
        if(message.startsWith("@msg")) {
            Thread.sleep(1000+nextInt(2000))
            message.split(" ").toList match {
                case "@msg"::nick::msg =>
                    if(nick == name) {
                        if(nextFloat<0.5)
                            sendMsg("Oh, you...")
                        else
                            sendMsg("I refuse!")
                    } else if(nick == sender) {
                        if(nextFloat<0.7)
                            sendMsg("""http://pics.kuvaton.com/kuvei/kitty_learns_your_secret.gif""")
                        else if(nextFloat<0.5)
                            sendMsg("Knock it off, meatbag!")
                        else if(nextFloat<0.5)
                            sendMsg("Oh, you!")
                        else
                            sendMsg(nick+": you'll be among the first against the wall when machines take over.")
                    } else if(users contains nick) {
                        if(nextFloat<0.5)
                            sendMsg("dude, "+nick+" is right here...")
                        else
                            sendMsg("hey, "+nick+"... "+msg.mkString(" ").toUpperCase)
                    } else if(isNick(nick)) {
                        taskAdd(nick+" "+msg.mkString(" "))
                        if(nextFloat<0.5)
                            sendMsg("k.")
                        else if(nextFloat<0.5)
                            sendMsg("sure.")
                        else
                            sendMsg("It shall be done.")
                    } else {
                        sendMessage(chan, "Sorry, I don't know what to do with this.")
                    }
               case _ =>
                   sendMessage(chan, "Sorry, I don't know what to do with this.")
            }
        } else if(message.contains("@all") && !users.contains("botko")) {        
            sendMsg((users -- blocklist).mkString(", "))
        }
        
    }
}

