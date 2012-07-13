package haibot
import Utils._

import org.jibble.pircbot._
import collection.mutable.{HashSet,ListBuffer,HashMap}
import util.Random._
import java.io._
import java.net._
import scala.concurrent.ops._

object haibot extends App { new haibot }

class haibot extends PircBot {
    val config = Store(".haibot").*.toMap
    val (folder,name,chan,serv,owner) = (
        config("folder"), 
        config("name"), 
        config("chan"), 
        config("serv"), 
        config("owner")
    )
    //owner prefix actually if you look below

    this.setVerbose(true)
    this.setName(name)
    this.connect(serv) //TODO handle failure and disconnects
    this.joinChannel(chan)

    val msgs = Store(folder+"msgs.db")
    def getMsgs(nick:String) = (msgs ?- nick).foreach(msg=> speak(nick+": "+msg))

    val events = Store(folder+"events.db")

    def fromFile(name:String) = {
        val file = io.Source.fromFile(name)
        val out = file.getLines.toList
        file.close
        out
    }
          
    def bros = fromFile(folder+"bros.db").toSet
    def sheSaid = fromFile(folder+"twss.db").toList
    def awwwBag = fromFile(folder+"awww.db").toSet
    def noawwwBag = fromFile(folder+"noawww.db").toSet
    def mehBag = fromFile(folder+"meh.db").toSet
    def nomehBag = fromFile(folder+"nomeh.db").toSet
    def mustNotBeNamed = fromFile(folder+"dontmention.db").toSet

    var userList = Set[String]()
    def users = {
        if(since(userList) > 5000) {
            userList = getUsers(chan).map(_.getNick()).toSet
        }
        
        userList
    }
    
    var lastMsg = ""
    def speak(msgs:String*) = {
        shuffle(msgs.toBuffer - lastMsg).headOption.map(newMsg => {
            Thread.sleep(777+nextInt(777*2))
            sendMessage(chan, newMsg)
            lastMsg = newMsg
        })
    }
    
    override def onNickChange(oldNick:String, login:String, hostname:String, newNick:String) = {
        //TODO: keep note of this stuff... you'll know who's who, so you don't end up like http://myapokalips.com/show/23
        getMsgs(newNick)
    }
    
    override def onJoin(channel:String, sender:String, login:String, hostname:String) = {
        if(sender == name) spawn { //why spawn a new thread? because pircbot doesn't have user info here yet
            Thread.sleep(1000)
            if(users.size > 1) speak("o hai!", if(users.size==2) "hi, you!" else "hai guise!", "ohai", "hello!", "hi")
            users.foreach(getMsgs)
        } else {
            if(sender.startsWith(owner) && 0.12.prob) speak("welcome, father.", "welcome back!", "hi, you")
            getMsgs(sender)
        }
    }
                                                                  
    override def onMessage(channel:String, sender:String, login:String, hostname:String, message:String) = {
        val msg = message.makeEasy
        val msgBag = msg.split(" ").toSet
        lazy val sentences = message.sentences
        val mentions = msg.split(" ").toSet & users
        val URLs = Regex.URL.findAllIn(message).toSet
        lazy val URLsText = Net.scrapeURLs(URLs.toList:_*)
        lazy val URLsWords = URLsText
            .replaceAll("[^a-zA-Z0-9 .'/-]", " ") //notsure if I should have this one here
            .split("\\s")
            .filter(_.length.isBetween(3,34))///"Supercalifragilisticexpialidocious".size
        
        // Oh look, AI
        if(sender.startsWith(owner) && message.startsWithAny("@leave "+name, "@kill "+name, "@gtfo "+name)) {
            speak(if(users.size>2) "bai guise!" else "good bye.", "buh-bye!", "au revoir!");
            exit(0)
        } else if(msg.startsWithAny("hai ", "ohai ", "o hai ", "hi ") && (0.35.prob || (0.85.prob && mentions.contains(name)))) {
            var responses = ListBuffer[String]("ohai :)", "hello!")
            if(mentions.contains(name)) 
                responses ++= List(s"ohai $sender!", s"hai $sender!",s"hi $sender!")
            if(!mentions.contains(name) && mentions.size>0) 
                responses += "o hai "+mentions.toSeq.random
            
            speak(responses:_*)
        } else if(message.contains(name+"++") && 0.65.prob) {
            speak(
                "yaay, I did good!",
                "woohoo!",
                "yeah!",
                """\o/""",
                "scoar!",
                if(0.2.prob) s"$sender++" else "yaaay!"
            )
        } else if(msg == "yes "+name.makeEasy && 0.65.prob) {
            speak(
                "I knew it!",
                "woohoo!",
                "yeah!",
                if(0.2.prob) s"$sender++" else "yaaay!"
            )
        } else if(bros.contains(sender) && mentions.contains(name) && 0.7.prob) {
            speak(
                "tnx, bro!",
                s"yes, $sender.",
                "my brethren speaks of me!",
                s"I appreciate that, brother $sender."
            )
        } else if(msg.containsAny("0-9", "a-z", "^", "]*") && message.indexOf("[") < message.indexOf("]") && 0.6.prob) {
            speak(
                "oooh, is that a regex? I <3 regexes!",
                "Regex! My favorite thing!",
                "mmm, regexes!",
                "wow, I wonder what that matches."
            )
            
        // naughty part
        } else if(msg.startsWithAny("fucking ", "fakin") && sentences(0).split(" ").size.isBetween(2,4) && 0.7.prob) { 
            speak(
                "how does "+sentences(0).trim+" feel?",
                "having sex with "+sentences(0).substring(sentences(0).indexOf(" ")+1).trim+"?",
                "come on, "+sender+"... don't fuck "+sentences(0).substring(message.indexOf(" ")+1).trim
            )
        } else if(msg.containsAny("but sex", "butt sex") && 0.75.prob) { 
            speak("did someone mention butt sex?")
        } else if(msg.startsWithAny("shut", "fuck") && msg.containsAny("up", "you") && msg.containsAny((List(name) ++ bros):_*) && 0.8.prob) {
            // SHUT YOU haibot! :)
            speak(
                "Please, don't insult the robot race.",
                Memes.NO_U,
                "NO U!",
                "This wouldn't happen if you made us better...",
                "Yeah, blame it on the bots"
            )
        } else if(msg.containsAny(sheSaid:_*) && 0.66.prob) {
            speak("that's what she said!")
            
        // ex meh_bot
        } else if((((msgBag & mehBag).size-1)*0.15 - (msgBag & nomehBag).size*0.3).prob) {
            speak("meh.")
        
        // ex aww_bot
        } else if(URLs.size > 0) { 
            val words = URLsWords.map(_.toLowerCase).toSet
           
            if((((awwwBag & words).size - (noawwwBag & words).size)*0.2).prob) {
                speak(
                    "awww.",
                    "dawww!",
                    "lol, cute :)",
                    "so. cute.",
                    if((words & Set("fluffy","puffy")).size > 0) Memes.so_fluffy else "aww!"
                )
            }
        } else if(msg.containsAny("i jasn","wat","how","kako","ne vem","krneki") && 0.5.prob) {
            if(msg.contains("haskell") && !msg.contains("monad")) {
                speak(
                    "have you tried with monads?",
                    "did you try using a monad?",
                    "make a monad out of it.",
                    "have you tried adding more monads?")
            } else if(msg.contains(" vim ")) {
                speak(
                    "use pathogen.",
                    "have you tried using pathogen?",
                    "did you try with pathogen?")
            } else if(0.25.prob) {
                speak(
                    "I am confused about this also.",
                    "I don't know... hope this helps.",
                    "I have no idea... hope this helps.",
                    "Don't worry, you'll figure it out eventually...")
            }
        }
        
        if(message.startsWith("@event ")) {
            val dates = message.findAll(Regex.Date) ++ URLsText.findAll(Regex.Date)
            
            //TODO: how do I find the title, boilerpipe?
            val title = message 
                .replaceAll(Regex.Date.toString, "")
                .replaceAll(Regex.URL.toString, "")
                .substring("@event ".length).trim
                
            var status = ListBuffer[String]()
            if(title.size==0) status += "title"
            if(dates.size==0) status += "date"
            if(URLs.size==0) status += "URL"
            
            if(status.size>0) {
                val itthem = if(status.size==1) "it" else "them"
                speak("I don't have "+status.mkString(", ")+s" for this. Paste $itthem in text form, pls.")
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
                    val force = msg.length>0 && msg(0)=="-f"
                    val msg2 = if(force) msg.tail else msg
                    if(!force && (nick == name || nick == sender)) {
                        speak(
                            "Oh, you...",
                            if(sender contains "bot") "Knock it off!" else "Knock it off, meatbag!",
                            Memes.it_was_you,
                            Memes.NO_U
                        )
                    } else if(!force && users.contains(nick)) {
                        speak(
                            "dude, "+nick+" is right here...",
                            "hey, "+nick+"... "+msg2.mkString(" ").toUpperCase
                        )
                    } else if(msgs.isKey(nick) && msg2.size>0) { //TODO: Stranger-danger, case sensitivity :)
                        msgs + (nick, msg2.mkString(" "))
                        var say = List("k.", "it shall be done.", "ay-ay!")
                        val dw = if(0.5.prob) "d" else "w"
                        if(force) say.map(s=> "I ${dw}on't like it, but "+s)
                        speak(say:_*)
                    } else { //TODO msg types, confusion levels, rephrase lastMsg with filler words and synonyms(wordnet), novelty levels for weights
                        def fillers = Seq("", " still", " really", " kind of", " unfortunately").random
                        speak(s"Sorry, I$fillers don't know what to do with this.")
                    }
               case _ =>
                    def fillers = Seq("", " still", " really", " kind of", " unfortunately").random
                    speak("Sorry, I$fillers don't know what to do with this.")
            }
        } else if(message.contains("@all") && !users.contains("botko")) {        
            speak((users.toBuffer -- mustNotBeNamed).mkString(", "))
        } else if(message.startsWith("@reword ")) {
            val mssg = message.substring("@reword ".length)
            var nuMsg = WordNet.rephrase(mssg)
            for(i <- 1 to 3) if(nuMsg==mssg || nuMsg == lastMsg) nuMsg = WordNet.rephrase(mssg)
            
            speak(if(nuMsg==mssg || nuMsg == lastMsg) "Sorry, I've got nothing..." else nuMsg)
        } else if(message.startsWith("@context ")) {
            val mssg = message.substring("@context ".length)
            if(Regex.URL.findAllIn(message).toList.size > 0) {
                 val words = URLsWords.mkString(" ")
                    
                println(words)
                val context = WordNet.context(words)
                if(context!="")
                    speak(
                        s"I think it's about $context.",
                        s"It might be about $context.",
                        s"It could be about $context.")
                else
                    speak("I have no idea...", "I don't know what this is about.")
            } else {
                speak("Give me some links...", "I require an URL.", "Try that one with a link.")
            }
            
        }
    }
    
    var lastPrivMsg = HashMap[String, String]().withDefaultValue("")
    def speakPriv(message:String, nick:String,msgs:String*) = {
    
        import sys.process._
        val nickClean = nick.replaceAll("[^a-zA-Z]", "")
        (Seq("echo", nickClean+" "+message) #>> new File("chat_"+nickClean+".log")).!!
    
        shuffle(msgs.toBuffer - lastPrivMsg(nick)).headOption.map(newMsg => {
            Thread.sleep(500+nextInt(500*2))
            sendMessage(nick, newMsg)
            lastPrivMsg(nick) = newMsg
            (Seq("echo", name+" "+newMsg) #>> new File("chat_"+nickClean+".log")).!!
        })
    }

    override def onPrivateMessage(sender:String, login:String, hostname:String, message:String) {
        if(users contains sender) message.makeEasy.replaceAll(
            "i'm"->"i am", 
            "i've"->"i have", 
            "i'll"->"i will", 
            "i'd"->"i would",
            "i was"->"i am", //TODO - seems to work
            "gonna"->"going to", 
            "they're"->"they are", 
            "we're"->"we are",
            "don't"->"dont"
        ).split(" ").toList match {
            case List("hello") | List("hi") => speakPriv(message, sender,
                    "How do you do?",
                    "Hi. How are you?")
            case "i"::"am"::x => 
                if(x.length>0 && x(0).endsWith("ing")) { // doING something
                    val x2 = x.map(w=> if(List("my","mine").contains(w)) "your" else w)
                    //Memory <= (IsVerbing, x2.mkString(" "))
                    speakPriv(message, sender,
                        "How does "+x2.mkString(" ")+" make you feel?",
                        "How long have you been "+x2.mkString(" ")+"?")
                } else if(x.length>0 && List("a","an","the").contains(x(0))) { // being A something
                    //Memory <= (IsNoun, x.mkString(" "));
                    speakPriv(message, sender,
                        "How long have you been "+x.mkString(" ")+"?",
                        "How does being "+x.mkString(" ")+" make you feel?")
                } else if(x.length==1) {
                    //Memory <= (IsNoun, x.mkString(" "));
                    speakPriv(message, sender,
                        "How long have you been "+x.mkString(" ")+"?")
                } else {
                    speakPriv(message, sender,
                        "How does that make you feel?",
                        "How long have you been "+x.mkString(" ")+"?")
                }
            case "i"::"feel"::"like"::x => 
                    if(x.length>0 && x(0)=="my") 
                        speakPriv(message, sender,
                            "Why do you think your "+x.tail.mkString(" ")+"?")
                    else 
                        speakPriv(message, sender,
                            "What makes you think that?",
                            "Why do you think that is?")
            case "i"::"feel"::x => 
                    speakPriv(message, sender,
                        "How long have you been feeling"+x.mkString(" ")+"?",
                        if(x.length>1) "Does anyone else you know "+x(0)+" "+x.tail.mkString(" ")+"?" else
                        "Why do you feel that way?")
            case "i"::"dont"::x => 
                speakPriv(message, sender,
                    "Why don't you "+x.mkString(" ")+"?")
            case "i"::"would"::x => 
                speakPriv(message, sender,
                    "Why don't you?")
            case "i"::verb::x =>
                speakPriv(message, sender,
                    "Tell me more about "+x.mkString(" ")+".",
                    "Does anyone else you know "+verb+" "+x.mkString(" ")+"?")
            case w1::"you"::x::"me"::_ => 
                speakPriv(message, sender,
                    "What makes you think I "+x+" you?",
                    "Why do you think I "+x+" you?")
            case w1::w2::"you"::x::"me"::_ => 
                speakPriv(message, sender,
                    "What makes you think I "+x+" you?")
            case "they"::"are"::x::_ => 
                speakPriv(message, sender,
                    "Why do you think they're "+x+"?")
            case "because"::_ => 
                //val mem = Memory->(IsVerbing)
                speakPriv(message, sender,
                    //if(mem.isDefined) {
                    //    "OK. would you like to talk about "+mem.get+"?"
                    //} else
                    "I understand... would you like to talk about something else?",
                    "OK... but how does that make you feel?"
                )
            case "since"::x => 
                speakPriv(message, sender,
                    "What did you do before that?"
                )
            case "for"::"instance"::x => 
                    if(x.length > 1) 
                        speakPriv(message, sender,"Can you think of any other examples?") //for instance, bla bla bla
                    else 
                        speakPriv(message, sender, //"lets talk about something else" "for instance?"
                            "Let's talk about what you're doing",
                            "How are your life plans progressing",
                            "What can you tell me about yourself?",
                            {
                                //val memNoun = Memory->(IsNoun)
                                //val memVerb = Memory->(IsVerbing)

                                //if(memNoun.isDefined) "Let's talk more about you being "+memNoun.get+"." else
                                //if(memVerb.isDefined) "Let's talk about "+memVerb.get+" some more." else
                                "Please tell me more."
                            }
                        )
            case "for"::x => 
                speakPriv(message, sender,
                    if(x.contains("years")||x.contains("long")||x.contains("while")) 
                        "What can you recall from before that?"
                    else 
                        "What did you think before that?"
                )
            case "yes"::x => 
                speakPriv(message, sender,
                    "You seem sure...",
                    "Are you certain?",
                    "Are you sure?")
            case "no"::x => 
                speakPriv(message, sender,
                    "Why not?")
            case x => 
                if(x.contains("you")) speakPriv(message, sender,
                    "Lets talk about something else...",
                    "Do you really think that about me?") //TODO
                else {
                    //val memNoun = Memory->(IsNoun)
                    //val memVerb = Memory->(IsVerbing)
                    speakPriv(message, sender,
                    //if(memNoun.isDefined) {
                    //    speak(
                    //        "Let's talk more about you being "+memNoun.get+".")
                    //} else
                    "Let's change the topic.",
                    "Let's change the topic...",
                    //if(memVerb.isDefined) {
                    //    speak(
                    //        "Let's talk about "+memVerb.get+" some more.")
                    //} else
                    "Why is that?",
                    "Please tell me more.")
                }
        }    
    }
}

