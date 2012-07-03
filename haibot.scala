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
    
    val msgs = Store(folder+"msgs.db")
    def getMsgs(nick:String) = (msgs ?- nick).foreach(msg=> speak(nick+": "+msg))

    val events = Store(folder+"event.db")

    def badExts = io.Source.fromFile(folder+"badexts.db").getLines.toBuffer
    def sheSaid = io.Source.fromFile(folder+"twss.db").getLines.toBuffer
    def awwwBag = io.Source.fromFile(folder+"awww.db").getLines.toSet
    def noawwwBag = io.Source.fromFile(folder+"noawww.db").getLines.toSet
    def mehBag = io.Source.fromFile(folder+"meh.db").getLines.toSet
    def nomehBag = io.Source.fromFile(folder+"nomeh.db").getLines.toSet
    def blocklist = io.Source.fromFile(folder+"block.db").getLines.toSet
    
    var lastMsg = ""
    def speak(msgs:String*) =
        shuffle(msgs.toBuffer - lastMsg).headOption.map(newMsg => {
            Thread.sleep(777+nextInt(777*2))
            sendMessage(chan, newMsg)
            lastMsg = newMsg
        })

    
    override def onNickChange(oldNick:String, login:String, hostname:String, newNick:String) = {
        getMsgs(newNick)
    }
    
    override def onJoin(channel:String, sender:String, login:String, hostname:String) = {
        if(sender == name) spawn {
            speak("o hai!", "hai guise!", "hello!", "I'm back")
            Thread.sleep(2000)
            users.foreach(getMsgs)
        }
        if(sender.startsWith("Hairy") && nextFloat<0.125) {
            speak("welcome, father.", "welcome back!")
        }
        getMsgs(sender)        
    }
                                                                  
    override def onMessage(channel:String, sender:String, login:String, hostname:String, message:String) = {
        val msg = message.makeEasy
        //TODO: make language for this kind of thing
        if(sender.startsWith("Hairy") && message.startsWith("@kill "+name)) {
            speak("bai guise!", "bye.", "au revoir!", "I understand.", "I'll be back");
            exit(0)
        } else if(msg.startsWithAny("hai ", "ohai ", "o hai ", "hi ") && (nextFloat<0.4 || (nextFloat<0.8 && msg.contains(name)))) {
            speak(
                s"hai $sender!",
                "ohai :)",
                "hello!",
                s"hi! $sender"
            )
        } else if(message.contains(name+"++") && nextFloat<0.65) {
            speak(
                "yaay, I did good!",
                "woohoo!",
                "yeah!",
                """\o/""",
                "scoar!",
                if(nextFloat<0.2) s"$sender++" else "yaaay!"
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
                WordNet.rephrase("Please, don't insult the robot race."),
                Memes.NO_U,
                "NO U!",
                WordNet.rephrase("This wouldn't happen if you made us better..."),
                "Yeah, blame it on the bots"
            )
        } else if(msg.containsAny(sheSaid:_*) && nextFloat<0.66) {
            speak("that's what she said!")
        } else if(Regex.URL.findAllIn(message).toList.size > 0) { //ex aww_bot
            val words = message
                .findAll(Regex.URL)
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
           
            if(nextFloat<0.22*((awwwBag & words).size - (noawwwBag & words).size)) {
                speak(
                    "awww.",
                    "dawww!",
                    "lol, cute :)",
                    "so. cute.",
                    if((words & Set("fluffy","puffy")).size > 0) Memes.so_fluffy else "aww!"
                )
            }
        } else if(nextFloat<(msg.split(" ").toSet & mehBag).size*0.17) { //ex meh_bot //TODO: unmeh for cool stuff
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
        
        if(message.startsWith("@event ")) {//TODO: request dialog needed here - can't just wing it
            var URLs = message.findAll(Regex.URL).distinct
            //TODO: detect date and title from this
            var text = (List("") ++ URLs.map(url => {
                var out = ""
                if(!url.endsWithAny(badExts:_*)) try {
                    out = KeepEverythingExtractor.INSTANCE.getText(new URL(url))
                } catch { case _ => }
                out
            })).reduce(_+_)
            println(text)
            var dates = message.findAll(Regex.Date) ++ text.findAll(Regex.Date)
            val title = message
                .replaceAll(Regex.Date.toString, "")
                .replaceAll(Regex.URL.toString, "")
                .substring("@event".length).trim
                
            var status = ListBuffer[String]()
            if(title.length==0) status += "title"
            if(dates.length==0) status += "date"
            if(URLs.length==0) status += "URL"
            
            if(status.length>0) {
                speak("I don't have "+status.mkString(", ")+" for this. Paste them in text form, pls")
            } else {
                speak("Is this right? "+dates(0)+" -- "+title)
                events + (dates(0).replaceAll("[/.]","_"), title+" "+URLs.mkString(" "))
            }
        } else if(message.startsWith("@events")) {
            events.*.foreach(event => 
                speak(event._1 + (if(event._2!=null) " "+event._2 else ""))
            )
        } else if(message.startsWith("-event ")) {
            var rem = events ?- message.substring("-event ".length) //TODO: lolwat
            speak("I have removed "+rem.length+" event"+(if(rem.length!=1) "s" else ""))
        } else if(message.startsWith("@msg ")) {
            message.split(" ").toList match {
                case "@msg"::nick::msg =>
                    val force = msg.length>0 && msg(0)!="-f"
                    val msg2 = if(force) msg.tail else msg
                    if(!force && (nick == name || nick == sender)) {
                        speak(
                            "Oh, you...",
                            "Knock it off, meatbag!",
                            Memes.it_was_you,
                            Memes.NO_U
                        )
                    } else if(!force && users.contains(nick)) {
                        speak(
                            "dude, "+nick+" is right here...",
                            "hey, "+nick+"... "+msg2.mkString(" ").toUpperCase
                        )
                    } else if(msgs.isKey(nick) && msg.size>0) { //TODO: Stranger-danger, case sensitivity :)
                        msgs + (nick, msg2.mkString(" "))
                        var say = List("k.", "it shall be done.", "ay-ay.")
                        val dw = if(nextFloat<0.5) "d" else "w"
                        if(force) say.map(s=> "I ${dw}on't like it, but "+s)
                        speak(say:_*)
                    } else { //TODO msg types, confusion levels, rephrase lastMsg with filler words and synonyms(wordnet), novelty levels for weights
                        val fillerList = List(" still", " really", " kind of", " unfortunately")
                        val filler = if(lastMsg.startsWith("Sorry")) shuffle(fillerList).toList(0) else ""
                        speak(s"Sorry, I$filler don't know what to do with this.")
                    }
               case _ =>
                   speak("Sorry, I don't know what to do with this.")
            }
        } else if(message.contains("@all") && !users.contains("botko")) {        
            speak((users.toBuffer -- blocklist).mkString(", "))
        } else if(message.startsWith("@reword ")) {
            var nuMsg = WordNet.rephrase(message.substring("@reword ".length))
            for(i <- 1 to 3) {
                if(nuMsg==message || nuMsg == lastMsg) nuMsg = WordNet.rephrase(message.substring("@reword ".length))
            }
            
            speak(
                if(nuMsg==message || nuMsg == lastMsg) "Sorry, I've got nothing..." else nuMsg
            )
        }
    }
}

