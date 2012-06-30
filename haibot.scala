package haibot
import Utils._

import org.jibble.pircbot._
import collection.mutable._
import util.Random._
import java.io._
import java.net._
import scala.concurrent.ops._
import de.l3s.boilerpipe.extractors._

object haibot extends App { new haibot }

class haibot extends PircBot {
    val settings = Store(".haibot")
    // TODO: make these def, and autoreload
    val folder = (settings ? "folder")(0)
    val name = (settings ? "name")(0)
    val chan = (settings ? "chan")(0)
    val serv = (settings ? "serv")(0)

    this.setVerbose(true)
    this.setName(name)
    this.connect(serv)
    this.joinChannel(chan)
    
    val userList = ListBuffer[String]()
    def users = {
        if(since(userList) > 5000) {
            userList.clear
            userList ++= getUsers(chan).map(_.getNick())
        }
        userList
    }
    
    val tasks = Store(folder+"tasks.db")
    def getTasks(nick:String) = (tasks ?- nick).foreach(task=> speak(nick+": "+task))

    def badExts = io.Source.fromFile(folder+"badexts.db").getLines.toBuffer
    def sheSaid = io.Source.fromFile(folder+"twss.db").getLines.toBuffer
    def awwwBag = io.Source.fromFile(folder+"awww.db").getLines.toSet
    def mehBag = io.Source.fromFile(folder+"meh.db").getLines.toSet
    def blocklist = io.Source.fromFile(folder+"block.db").getLines.toSet
    
    var lastMsg = ""
    def speak(msgs:String*) =
        shuffle(msgs.toBuffer - lastMsg).headOption.map(newMsg => {
            Thread.sleep(777+nextInt(777*2))
            sendMessage(chan, newMsg)
            lastMsg = newMsg
        })

    
    override def onNickChange(oldNick:String, login:String, hostname:String, newNick:String) = {
        getTasks(newNick)
    }
    
    override def onJoin(channel:String, sender:String, login:String, hostname:String) = {
        if(sender == name) spawn {
            speak("o hai!", "hai guise!", "hello!", "I'm back")
            Thread.sleep(2000)
            users.foreach(getTasks)
        }
        if(sender.startsWith("Hairy") && nextFloat<0.125) {
            speak("welcome, father.", "welcome back!")
        }
        getTasks(sender)        
    }
                                                                  
    override def onMessage(channel:String, sender:String, login:String, hostname:String, message:String) = {
        val msg = message.makeEasy
        //TODO: make language for this kind of thing
        if(sender.startsWith("Hairy") && message.startsWith("@kill "+name)) {
            speak("bai guise!", "bye.", "au revoir!", "I understand.", "I'll be back");
            exit(0)
        } else if(sender=="_mehbot_" && nextFloat<0.10) { //TODO: remove if merge becomes final
            speak(
                "show some enthusiasm, _mehbot_...",
                "don't listen to _mehbot_... tell us more!"
            )
        } else if((msg.startsWithAny("hai ", "ohai ", "o hai ", "hi ") && nextFloat<0.4)
            || (msg.startsWithAny("hai ", "ohai ", "o hai ", "hi ") && msg.contains(name) && nextFloat<0.85)) {
            speak(
                s"hai $sender!",
                "ohai :)",
                "hello!",
                s"hi! $sender"
            )
        } else if(message.contains(name+"++") && nextFloat<0.7) {
            speak(
                "yaay, I did good!",
                "woohoo!",
                "yeah!",
                """\o/""",
                "scoar!"
            )
        } else if(sender=="botko" && message.contains(name)) {
            speak(
                "my brethren speaks of me!",
                "I appreciate that, brother botko."
            )
        } else if(msg.containsAny("0-9", "a-z", "^", "]*") && message.indexOf("[") < message.indexOf("]") && nextFloat<0.6) {
            speak(
                "oooh, is that a regex? I <3 regexes!",
                "Regex! My favorite thing!",
                "mmm, regexes!",
                "wow, I wonder what that matches."
            )
        } else if(msg.startsWithAny("fucking ", "fakin") && msg.sentences(0).split(" ").length < 5 && nextFloat<0.7) {
            speak(
                "how does "+message.sentences(0)+" feel?",
                "having sex with "+message.sentences(0).substring(message.indexOf(" ")+1)+"?",
                "come on, "+sender+"... don't fuck "+message.sentences(0).substring(message.indexOf(" ")+1)+"?"
            )
        } else if(msg.startsWithAny("shut", "fuck") && msg.containsAny("up", "you") && msg.containsAny(name, "botko", "bot_") && nextFloat<0.8) {
            speak(
                "Please, don't insult the robot race.",
                Memes.NO_U,
                "NO U!",
                "This wouldn't happen if you made us better...",
                "Yeah, blame it on the bots"
            )
        } else if(msg.containsAny(sheSaid:_*) && nextFloat<0.66) {
            speak("that's what she said!")
        } else if(Regex.URL.findAllIn(message).toList.size > 0) { //ex aww_bot
            val words = Regex.URL
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
                speak(
                    "awww.",
                    "dawww!",
                    "lol, cute :)",
                    "so. cute.",
                    if((words & Set("fluffy","puffy")).size > 0) Memes.so_fluffy else "aww!"
                )
        } else if(nextFloat<(msg.split(" ").toSet & mehBag).size*0.17) { //ex meh_bot
            speak("meh.")
        } else if(msg.containsAny("i jasn","wat","how","kako","ne vem","krneki") && nextFloat<0.2) {
            if(msg.contains("haskell") && !msg.contains("monad")) {
                speak(
                    "have you tried with monads?",
                    "did you try using a monad?",
                    "make a monad out of it.",
                    "have you tried adding more monads?"
                )
            } else if(msg.contains(" vim ")) {
                speak(
                    "use pathogen.",
                    "have you tried using pathogen?",
                    "did you try with pathogen?"
                )
            }
        }
        
        if(message.startsWith("@msg")) {
            message.split(" ").toList match {
                case "@msg"::nick::msgs =>
                    if(nick == name || nick == sender) {
                        speak(
                            "Oh, you...",
                            "Knock it off, meatbag!",
                            Memes.it_was_you,
                            Memes.NO_U
                        )
                    } else if(users contains nick) {
                        speak(
                            "dude, "+nick+" is right here...",
                            "hey, "+nick+"... "+msgs.mkString(" ").toUpperCase
                        )
                    } else if(tasks.isKey(nick)) {
                        tasks + (nick, msgs.mkString(" "))
                        speak("k.", "I'll do it.", "It shall be done.", "ay-ay.")
                    } else {
                        speak("Sorry, I don't know what to do with this.")
                    }
               case _ =>
                   sendMessage(chan, "Sorry, I don't know what to do with this.")
            }
        } else if(message.contains("@all") && !users.contains("botko")) {        
            speak((users.toBuffer -- blocklist).mkString(", "))
        }        
    }
}

