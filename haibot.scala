package org.psywerx

import org.psywerx.util._
import org.psywerx.Time._
import org.psywerx.Caption.C

import org.jibble.pircbot._
import collection.mutable.{HashSet,ListBuffer,HashMap}
import scala.util.Random._
import java.io._
import java.net._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.util._
import scala.concurrent.duration._
import sys.process._

object haibot extends App { new haibot }

class haibot extends PircBot {
  var startTime = now
  val joinTimes = HashMap[String, Int]().withDefaultValue(-1)
  
  val config = Store(".config").toMap
  val (folder,login,name,chan,serv,owner) = (
    config("folder"), 
    config("login"), 
    config("name"), 
    config("chan"), 
    config("serv"), 
    config("owner")) //owner prefix actually
  
  this.setVerbose(true)
  this.setLogin(login)
  this.setName(name)
  this.connect(serv) //TODO handle failure and disconnects
  this.joinChannel(chan)

  val apiKeys = Store(".apikeys").toMap

  lazy val wordnet = new WordNet("lib/WordNet/")
  lazy val zemanta = new Zemanta(apiKeys("Zemanta"))
  lazy val bitly = new Bitly(apiKeys("bitly1"), apiKeys("bitly2"))
  lazy val novatel = new NovatelSMS(apiKeys("novatel_user"), apiKeys("novatel_pass"))
  val smsCoolDown = 77
  var lastSMSTime = 0

  val events = Store(folder+"events.db")

  // TODO: cache them at least for a minute or so
  def sheSaid = getFile(folder+"twss.db").toList
  def awwwBag = getFile(folder+"awww.db").toSet
  def noawwwBag = getFile(folder+"noawww.db").toSet
  def mehBag = getFile(folder+"meh.db").toSet
  def nomehBag = getFile(folder+"nomeh.db").toSet
  def mustNotBeNamed = getFile(folder+"dontmention.db").toSet
  def girls = getFile(folder+"girls.db").map(_.cleanNick).toSet
  def trusted = getFile(folder+"trusted.db").map(_.cleanNick).toSet
  def bots = getFile(folder+"bots.db").map(_.cleanNick).toSet
  
  implicit class IrcString(val s: String) { 
    def cleanNick: String = s.toLowerCase.replaceAll("[0-9_]|-nexus$","")
    def isGirl = girls.contains(s.cleanNick)
    def isTrusted = trusted.contains(s.cleanNick)
    def isBot = (s.startsWith("_") && s.endsWith("_")) || bots.contains(s.cleanNick)
  }
  
  var lastMsgs = List[String]()
  
  var twitterCheck = 0
  val twitterCheckInterval = 7*60
  def checkTwitter(force: Boolean = false) {
    if(since(twitterCheck) > twitterCheckInterval || force) {
      val mentions = Seq("t", "mentions", "-n", "5").!!.trim
      if(mentions(0) == '@') {
        val lastTweets = getFile(folder+"lasttweets.db")
        val mentionList = mentions.replaceAll("\n   ", " ").split("\n").take(5).map(_.trim).takeWhile(tw => !(lastTweets contains tw))
        if(mentionList.nonEmpty) {
          // Save the last few mentions
          val newLastTweets = (mentionList ++ lastTweets).take(10)
          printToFile(folder+"lasttweets.db")(newLastTweets.mkString("\n"))

          // Speak the new mentions
          for(mention <- mentionList) {
            val (name, msg) = mention.splitAt(mention.indexOf(" "))
            speak(name.drop(1) + " on Twitter says:" + msg)
          }
        }
      }
      twitterCheck = now
    }
  }
  
  //TODO: this is horrible and error-prone
  var tweetScore = Set[String]()
  var tweetPlsScore = Set[String]()
  var tweetNegScore = Set[String]()
  var tweetMsg = ""
  var tweetId = ""
  var tweetLim = 2
  var tweetNames = Array[String]()

  var userList = Set[String]()
  def users: Set[String] = {
    if(since(userList) > 5) {
      userList = getUsers(chan).map(_.getNick).toSet
    }
    
    userList
  }
  
  var lastMsg = ""
  def speakNow(msgs: String*) {
    for(newMsg <- (msgs.toBuffer - lastMsg).randomOption.orElse(Some(lastMsg))) {
      sendMessage(chan, newMsg)
      lastMsg = newMsg
    }
  }
  def speak(msgs: String*) {
    Thread.sleep(777 + nextInt(777*2))
    speakNow(msgs: _*)
  }
  val msgs = Store(folder+"msgs.db")
  // Fetch messages for nick and speak them
  def speakMessages(nick: String) {
    val now = (new java.util.Date).getTime
    for(rawMsg <- msgs ?- nick.toLowerCase) {
      //TODO: other kind of params, also augh!
      val (param, msg) = rawMsg splitAt (rawMsg indexOf ",") match { case (s1,s2) => (s1.toLong, s2.tail) }
      if(param > now) {
        msgs += (nick.toLowerCase, param+","+msg)
      } else {
        if(msg.startsWithAny("++", nick+"++"))
          speak(nick+"++" + msg.dropWhile(_ != '+').drop(2))
        else
          speak(nick+": "+msg)
      }
    }
  }
  
  spawn {
    while(true) {
      if(this.isConnected) {
        users.foreach(speakMessages)
      }
      Thread.sleep(20*1000)
    }
  }
  
  override def onNickChange(oldNick: String, login: String, hostname: String, newNick: String) {
    //TODO: keep note of this so you'll know who's who
    //speakMessages(newNick)
  }
  
  var shutdown = false
  override def onDisconnect() {
    println("onDisconnect: event triggered.")
    var backoff = 5000
    while(!this.isConnected && !shutdown) {
      try {
        this.reconnect()
        this.joinChannel(chan)
      } catch {
        case e: IOException => 
          println("onDisconnect: it was not possible to reconnect to the server.")
        case e: IrcException =>
          println("onDisconnect: the server would not let me join it.")
        case e: NickAlreadyInUseException =>
          println("onDisconnect: my nick is already in use on the server.")
          this.setName(name+"_")
      }
      if(backoff < 60000) backoff += 5000
      Thread.sleep(backoff)
    }
  }

  override def onJoin(channel: String, sender: String, login: String, hostname: String) {
    if(sender == this.name) spawn { //why spawn a new thread here? because pircbot doesn't have user info here yet, but does immediately after
      Thread.sleep(2000)
      startTime = now
      if(users.size > 1) speak("o hai!", if(users.size == 2) "hi, you!" else "hai guise!", "ohai", c"hello{!}", "hi", "hi there!")
      //users.foreach(speakMessages)
    } else {
      Thread.sleep(1000)
      if(sender.startsWith(owner) && 0.05.prob) speak(
        c"welcome, father{!|.}",
        c"welcome back{!|.}",
        c"hi, you{!}",
        c"I've missed you, $sender{.}")

      //speakMessages(sender)
      joinTimes(sender.toLowerCase) = now
    }
  }

  override def onMessage(channel: String, sender: String, login: String, hostname: String, message: String) {
    val msg = message.makeEasy
    val msgBag = msg.split(" ").toSet
    lazy val sentences = message.sentences
    val mentions = message.replaceAll("[,:]", " ").split(" ").toSet & (users ++ users.map(_.toLowerCase) ++ (users.map(_.toLowerCase) & bots).map(_.replaceAll("_", "")))
    val URLs = message.findAll(Regex.URL).distinct
    lazy val URLsText = Net.scrapeURLs(URLs.toList: _*)
    lazy val URLsWords = 
      URLsText
        .replaceAll("[^a-zA-Z0-9 .'/-]", " ") //notsure if I should have this one here
        .split("\\s")
        .filter(_.length.isBetween(3,34))///"Supercalifragilisticexpialidocious".size
    lazy val recentContext = 
      (URLsText + ". " + lastMsgs.mkString(". ") + ". " + message)
        .replaceAll(Regex.URL.toString, "")
        .replaceAll("@[a-z]+[ ,:]", "")
        .replaceAll("^[ .]+","")
        .trim
    
    // Oh look, AI
    if(sender.startsWith(owner) && message.startsWithAny("@leave "+name, "@kill "+name, "@gtfo "+name, "@die "+name)) {
      speakNow(if(users.size > 2) "bai guise!" else "good bye.", "buh-bye!", "au revoir!")
      shutdown = true
      sys.exit(0)
    } else if(msg.startsWithAny("hai ", "ohai ", "o hai ", "hi ", "'ello ", "ello ", "oh hai", "hello") && (0.35.prob || (0.9.prob && mentions.contains(name)))) {
      var hai = Seq("ohai", "o hai", c"[hello|hi] {there}")
      if(mentions.contains(name) && 0.7.prob) 
        hai = hai.map(_ + " " + sender)
      if(mentions.nonEmpty && !mentions.contains(name) && 0.7.prob) 
        hai = hai.map(_ + " " + mentions.toSeq.random)
        
      hai = hai.map(_ + " " + c"{:)|!|^_^}").map(_.replaceAll(" !", "")) //first new caption system fail :/
      
      speak(hai: _*)
    } else if(message.contains(name+"++") && 0.65.prob) {
      speak(
        c"woo{o}hoo{o}!",
        "yeah!",
        """\o/""",
        "scoar!",
        if(0.2.prob) s"$sender++" else "yaaay!")
    } else if((msg.startsWith("yes "+name) && 0.65.prob) || (msg.startsWith("yes ") && mentions.contains(name) && 0.3.prob)) {
      speak(
        "I knew it!",
        "woohoo!",
        "yeah!",
        if(0.2.prob) s"$sender++" else "yaaay!")
    } else if(sender.isBot && mentions.contains(name) && 0.7.prob) {
      speak(
        "tnx, bro!",
        s"yes, $sender.",
        "my brethren speaks of me!",
        s"I appreciate that, brother $sender.")
    } else if(message.containsAny("0-9", "a-z", "A-Z", "]*", ".*", ".+") && message.indexOf("[") < message.indexOf("]") && (0.5 + (if(message.containsAny("^", "$", "{", "}")) 0.15 else 0)).prob) {
      speak(
        c"ooo{o}h, is that a regex? I [<3|love] regexes[!]2",
        c"Regex{es}[!]2 {M|m}y favorite thing[!]2",
        c"mm{m}{m}, regex{es}{!|...}",
        c"{wow, }I wonder what that matches.{.}")
    } else if(msg.containsAny("tnx", "thanks", "hvala") && ((mentions.contains(name) && 0.8.prob) || (msg.contains(" alot ") && 0.5.prob))) {
      if(msg.contains(" alot ") && !msg.contains("hvala")&& 0.9.prob) {
        speak(Memes.thanks_alot)
      } else {
        speak(
          c"[your|you're|You are] welcome{, ${sender}}{.}",
          c"{for you, }any time{, ${sender}}{.}",
          c"[f|F]or you{, $sender}... any time{!}")
      }
      // naughty part
    } else if(msg.contains("i need an adult") && 0.9.prob) { 
      speak(c"I am an adult{!}{!}{!}")
    } else if(msg.startsWithAny("fucking ", "fakin") && sentences(0).split(" ").size.isBetween(2,5) && 0.75.prob) { 
      speak(
        "how does "+sentences(0).trim+" feel?",
        "having sex with "+sentences(0).substring(sentences(0).indexOf(" ")+1).trim+"?",
        "come on, "+sender+"... don't fuck "+sentences(0).substring(message.indexOf(" ")+1).trim)
    } else if(msg.containsAny("but sex", "but fuck") && 0.7.prob) { 
      speak(c"{has someone mentioned|did someone mention} butt sex?")
    } else if(msg.containsAny("shutup", "shut up", "fuck you", "damn") && ((mentions.contains(name) && 0.9.prob) || ((mentions & bots).nonEmpty && 0.8.prob))) {
      speak(
        c"U MAD, BRO?{ :P}",
        Memes.NO_U,
        "NO U!",
        "This wouldn't happen if you made us better"+"."*0~3,
        "Yeah, blame it on the bots"+"."*0~3)
    } else if(msg.containsAny(sheSaid: _*) && 0.66.prob) {
      speak("that's what she said!")
      
    // ex meh_bot
    } else if((((msgBag & mehBag).size-1)*0.15 - (msgBag & nomehBag).size*0.33).prob) {
      speak("meh.")
    
    // ex aww_bot
    } else if(URLs.nonEmpty) spawn {
      val words = URLsWords.map(_.toLowerCase).toSet
       
      if((((awwwBag & words).size - (noawwwBag & words).size)*0.2).prob) {
        speak(
          c"{d}[a]3[w]3-5{!}2",
          c"{lol, }{how }cute{!} [:)|^_^]",
          c"so{.} cute.",
          if((words & Set("fluff","puff")).nonEmpty) Memes.so_fluffy else "aww!")
      }
    } else if(msg.containsAny("i jasn","wat","how","kako","ne vem","krneki") && !(msg.contains("show")) && (0.45 + (if(message contains "?") 0.25 else 0)).prob) {
      if(msg.contains("haskell") && !msg.contains("monad")) {
        speak(
          "have you tried with monads?",
          "did you try using a monad?",
          "make a monad out of it.",
          "have you tried adding more monads?")
      } else if(msg.contains(" lisp")) {
        speak(
          "have you tried reversing the polarity of your polish notation?",
          "have you tried adding more parentheses?",
          "have you written any macros to address this?")
      } else if(msg.contains(" vim")) {
        speak(
          "use pathogen.",
          "have you tried using pathogen?",
          "did you try with pathogen?")
      } else if(msg.containsAny(" ljud", " folk","people") && 0.5.prob) {
        speak(c"{yeah,} people are [weird|strange]{, I guess}...")
      } else if((mentions contains this.name) && 0.9.prob) {
        speak(c"I can't tell if you're asking me, or asking about me.")
      } else if(0.27.prob || math.min(0.5, message.count(_ == '?') * 0.15).prob) {
        speak(
          c"[I am|I'm] [confused|not sure] about this[ also|, too]{.}3",
          c"This [puzzles|confuses] me {greatly|very much}[ also|, too]{.}3",
          c"[I don't know|I dunno|I have no idea|No idea]... hope [I've helped|this helps] {at least a little|in some way}{.}3",
          c"[I wouldn't|Don't] worry {about it} {so much|too much}, I'm sure you'll figure it out {or something} {eventually|with time|in time}{.}3",
          c"I guess {you could say} that is some[thing|what| kind] of a [conundrum|mystery]{.}3",
          (if(mentions.nonEmpty && 0.8.prob) 
             c"[I don't know|I have no idea][, |...] but {yes, }${mentions.toSeq.random} might."
           else
             c"[Have you tried|Did you try|Have you attempted|Did you attempt] [searching the|looking on the|querying the|inquiring upon the] [internet|intarweb|DERPAnet|ARPAnet|cyberspace|electronic noosphere|information super-highway|W3 Infobahn]{s}?"
          ))
      }
    }

    
    if(message.startsWithAny("@ocr ", "@read ")) {
      val imgurReg = """(https?://)(?:www[.])?(imgur.com/)(?:(?:gallery/)|(?:r/[a-z]+/))?([A-Za-z0-9]+)""".r
      def toImgurImg(url: String): String = url match {
        case imgurReg(protocol, domain, img) => protocol+"i."+domain+img+".png"
        case _ => url
      }
      val pics = URLs.map(toImgurImg).filter(_.endsWithAny(".jpg", ".jpeg", ".png"))
      if(pics.size < URLs.size) speakNow(c"{Sorry,} I only read jpegs and pngs.")
      
      for(pic <- pics) Net.withDownload(pic) {
        case Some(tempFile) =>
            speakNow(OCR.OCR(tempFile)
              .filter(_.split(" ").exists(_.size >= 3))
              .map(guess => List(s"I think it says: $guess", s"My best guess is: $guess"))
              .getOrElse(List(c"Sorry, {I have} no idea, but I{'m| am} still learning {how} to read.")):_*)
        case _ =>
          speak(c"I don't {even} know how to download this{ file}.")
      }
    } else if(message.startsWithAny("@shorten ", "@bitly ")) {
      if(URLs.nonEmpty) {
        val bitlyURLs = URLs.flatMap(bitly.shorten).mkString(" ").trim
        if(bitlyURLs.isEmpty) {
          speak("Sorry, couldn't shorten url...")
        } else if(bitlyURLs.size < URLs.size) {
          speak(c"Couldn't shorten all of these, but here{ you go}: $bitlyURLs")
        } else {
          speak(s"Here you go: $bitlyURLs", s"Here: $bitlyURLs")
        }
      } else {
        speak("Give me a link...", "I require an URL.", "Try that with a link.")
      }
    } else if(message.startsWith("@event ")) {
      val dates = getFutureDates(message) ++ getFutureDates(URLsText)
      //http://metabroadcast.com/blog/boilerpipe-or-how-to-extract-information-from-web-pages-with-minimal-fuss
      //TODO: how do I find the title, boilerpipe?
      val title = message 
        //.replaceAll(Regex.Date.toString, "")
        .replaceAll(Regex.URL.toString, "")
        .substring("@event ".length).trim
        
      val status = ListBuffer[String]()
      if(title.isEmpty) status += "title"
      if(dates.isEmpty) status += "date"
      if(URLs.isEmpty) status += "URL"
      
      if(status.nonEmpty) {
        val itthem = if(status.size == 1) "it" else "them"
        speak("I don't have "+status.mkString(", ")+s" for this. Paste $itthem in text form, pls.")
      } else {
        speak("Yeah, looks OK, but I'm not gonna save it anywhere or anything :)")
        //speak("Is this right? "+dates(0)+" -- "+title)
        //events + (dates(0).replaceAll("[/. ]","_"), title+" "+URLs.mkString(" "))
      }
    } else if(message.startsWith("@events")) {
     for(event <- events.*) speak(event._1 + (if(event._2 != null) " "+event._2 else ""))
    } else if(message.startsWith("-event ")) {
      val rem = events ?- message.substring("-event ".length) //TODO: lolwat
      speak("I have removed "+rem.length+" event"+(if(rem.length != 1) "s" else ""))
    // Twitter/FB part
    } else if(message.startsWithAny("@#", "#@")) {
      // Ignore
    } else if(message.startsWithAny("@yes", "@yep", "@sure", "@maybe", "@perhaps", "@please")) {
      if((message.startsWithAny("@yes", "@yep", "@sure") || (message.startsWithAny("@maybe", "@perhaps") && 0.5.prob)) && sender.isTrusted) tweetScore = tweetScore ++ Set(sender)
      
      var beggedBefore = false
      if(message.startsWith("@please")) {
        beggedBefore = (tweetPlsScore contains sender)
        if(sender.isTrusted) tweetPlsScore = tweetPlsScore ++ Set(sender)
      }
      if(beggedBefore && 0.4.prob) speak("Come on "+sender+", stop begging", "You may beg only once, "+sender+".")
      
      import sys.process._
      def overLimit: Boolean = (tweetScore.size-tweetNegScore.size >= tweetLim) || (!beggedBefore && message.startsWith("@please") && 0.25.prob)
      if(tweetMsg == null && tweetId == null && tweetNames.nonEmpty) {
        val ret = (Seq("t", "follow") ++ tweetNames).!
        if(ret == 0)
          speak("Follow'd!", "It's done", "It is done.")
        else
          speak("Failed to follow :/")
      
        tweetScore = Set()
        tweetNegScore = Set()
        tweetPlsScore = Set()
        tweetNames = Array()
      } else if(tweetMsg == null && (tweetId matches "[0-9]*") && overLimit) {
        val ret = Seq("t", "retweet", tweetId).!
        
        val successResponses = ListBuffer("Retweeted it!", "It's done", "It is done.", "I retweeted the tweet out of that tweet.")

        //TODO: cleanup on aisle 5 *ding ding*
        //try facebook too
        val tweetDetails = withAlternative(
          (Seq("t", "status", tweetId).!!)
            .split("\n")
            .map(line => line.splitAt("Screen name  ".length))
            .map(line => (line._1.trim, line._2))
            .toMap,
          Map[String,String]()
        ).withDefaultValue("")
        
        if(tweetDetails("Text") != "") {
        
          // fbcmd as 361394543950153 POST "" "HairyFotr" "http://www.twitter.com/HairyFotr/status/279256274632327168/" "via Twitter" "Showed a friends kid a half-made game demo yesterday - he played it for like an hour and thought-up dozens of ideas :) #kids"
          val ret2 = Seq("fbcmd", "AS", "361394543950153", "POST",
            "",
            " "+tweetDetails("Screen name"),
            "https://twitter.com/statuses/"+tweetId,
            "via Twitter",
            " "+tweetDetails("Text")).!
          val screenName = tweetDetails("Screen name")
          if(screenName.nonEmpty) {
            successResponses ++= List(
              c"""[I've r|R|r]etweeted {the tweet out of} [that|the] {lovely|nice} tweet by $screenName{!}""",
              c"I[ hope|'m hoping]"+s" $screenName is "+c"""[pleased with|happy for|happy about] this retweet{.}""")
          }
        }
        
        if(ret == 0)
          speak(successResponses: _*)
        else
          speak("Failed to retweet :/")

        tweetScore = Set()
        tweetNegScore = Set()
        tweetPlsScore = Set()
        tweetMsg = ""
      } else if(tweetMsg != null && tweetMsg.nonEmpty && tweetMsg.size <= 140 && overLimit) {
        val ret = Seq("t", "update", tweetMsg).!
        val ret2 = Seq("fbcmd", "AS", "361394543950153", "POST", tweetMsg).!
        
        (ret,ret2) match {
          case (0,0) => speak(c"It['s| is| has been] [posted|done]{.}", c"I['ve| have] done as you requested", c"I{'ve} tweeted it{, and facebook'd it}{!}", "Posted it.")
          case (0,_) => speak("Tweeted it, but facebooking failed!")
          case (_,0) => speak("Facebook'd it, but tweeting failed!")
          case (_,_) => speak("Failed to post anywhere :(")
        }

        tweetScore = Set()
        tweetNegScore = Set()
        tweetPlsScore = Set()
        tweetMsg = ""
      }
    } else if(message.startsWithAny("@no", "@nein", "@nah") || (message.startsWith("@meh") && 0.5.prob)) {
      tweetNegScore = tweetNegScore ++ Set(sender)
    } else if(message.startsWith("@checktweets") && sender.isTrusted) {
      checkTwitter(force = true)
    } else if(message.startsWith("@follow ")) {
      val names = message.drop("@follow ".length).replaceAll("@","").split(" ").distinct
      if(names forall { _ matches "^[A-Za-z0-9_]{1,20}$" }) {
        tweetMsg = null
        tweetId = null
        tweetNames = names
        tweetScore = Set(sender)
        tweetNegScore = Set()
        tweetPlsScore = Set()
        tweetLim = 2
        speak(
          "Someone pls confirm.",
          "Please confirm.",
          "I need a vote.",
          "Someone say @yes or @no.")
      } else {
        speak("Notsure about this... no.", "Noep, something's not right here")
      }
    } else if(URLs.exists(_ matches Regex.tweet) && !sender.isBot) {
      val Regex.tweet(tId) = URLs.find(_ matches Regex.tweet).head
      tweetMsg = null
      tweetId = tId
      tweetScore = Set(sender)
      tweetNegScore = Set()
      tweetPlsScore = Set()
      tweetLim = 2
      speak(
        c"A retweet of [this|that] tweet {, perhaps|, maybe}?",
        c"[Want me to|Should I|Am I to|Is it OK if I|Is it OK to] retweet [this|that] {tweet}?",
        c"I [can|could] retweet [this|that], if you [guise|guys|ppl|people] [confirm it|want me to|agree]{.}3",
        c"{Hey, that} looks like a tweet... [should I|do you want me to|want me to|would you like me to] retweet it?",
        c"If [one of you|someone] confirms [this|it], I'll retweet {it}{.}",
        c"Someone {please} confirm {this|this tweet}, and I'll retweet it{.}")
    } else if(message.startsWith("@world ")) {
      val tweet = {
        var out = message.drop("@world ".length).trim
        if(URLs.nonEmpty && out.size > 140) {
          var shortTweet = out
          for(url <- URLs) {
            val bitlyUrl = bitly.shorten(url)
            if(bitlyUrl.isDefined) shortTweet = shortTweet.replace(url: CharSequence, bitlyUrl.get: CharSequence)
          }
          println(s"Shortened tweet from ${out.size} to ${shortTweet.size}")
          out = shortTweet
        }
        
        out
      }
      
      if(tweet.size >= 1 && tweet.size <= 140) {
        val andGals = if((girls & users).nonEmpty) c"{and gals}" else ""
        speak(
          c"Some[one|body] {pls|please} confirm {, and I'll post it}{.}",
          c"Does anyone {else} {in here|here} think it's a good idea to [tweet|post] [this|this tweet|that]? {:)}",
          c"Anyone {else} {in here|here} thinks it's a good idea to [tweet|post] [this|this tweet|that]? {:)}",
          c"Do you [guise $andGals|people] agree that I should [tweet|post] [this|that]?",
          c"Do you [guise $andGals|people] agree that this should be [tweeted|posted]?",
          c"If [one of you|someone] confirms [this|it], I'll tweet {it}{.}",
          c"I need a vote{, before I post this}{.}",
          c"Someone {should} {simply} confirm this {, and I'll post it}{.}")
        tweetMsg = tweet
        tweetScore = Set(sender)
        tweetNegScore = Set()
        tweetPlsScore = Set()
        tweetLim = if(sender.isTrusted) 2 else 3
        if(tweetLim > 2) {
          speak("Also, I don't think I know you... I need "+(tweetLim-1)+" votes for you")
        }
      } else {
        speak("That's too long to tweet, you twit! ("+tweet.size+" char)")
      }
    }
    
    if((message startsWith "@smsreset") && (sender startsWith owner)) {
      lastSMSTime = 0
    } else if(message startsWith "@sms ") {
      val msg = message.drop("@sms ".size)
      val (name, sms) = msg.splitAt(msg.indexOf(" "))
      val nums = Store(folder+"numbers.db").toMap
      if(!sender.isTrusted) {
        speak(c"{Sorry, }you're not on the trusted user list{ (yet?)}.")
      } else if(name.trim.isEmpty || sms.trim.isEmpty) {
        speak(c"You['ve forgotten| forgot] the name or {possibly} the message.")
      } else if(nums contains name.toLowerCase) {
        if(since(lastSMSTime) < smsCoolDown) {
          speak(c"Wait {a bit|somewhat} longer before sending [another|the next] [message|sms|msg]{, please}{.}")
          lastSMSTime = now
        } else spawn {
          println("Sending sms: "+nums(name.toLowerCase)+" "+msg)
          val response = novatel.sendSMS(number = nums(name.toLowerCase), msg = sms.trim)
          println("Response sms: "+response)
          if(response == "Ok") {
            speak(
              c"I['ve| have] done it.",
              c"{o}k{.|, chief}",
              c"it['s| is| has been] done{.}", 
              c"ay{-ay} {cap'n|captain}!", 
              c"{sure,|ok,|ok yeah,|ay,} I['ll| will] relay the [msg|message]{.}")

            lastSMSTime = now
          } else {
            speak(c"{Hmm, }[weird|strange]... Novatel {server} says:"+" "+response)
          }
        }
      } else {
        def hisForm(nick: String): String = if(nick.isGirl) "her" else "his"
        speak(c"I don't have ${hisForm(name)} number.{.. and no, you can't tell me it :P} {Sorry about this}")
      }
    }
    
    val msgReg = """@(?:msg|tell|ask)(?:[(]([^)]*)[)])? ([-a-zA-Z0-9_,]*):? (.*)""".r
    if(message matches msgReg.toString) {
      message match {
        case msgReg(rawParam, rawNicks, rawMsg) =>
          //TODO: add other params, like onactive, etc.
          val param = (Option(rawParam) map { param => Time.getFutureDates(param) } emptyToNone) map { _.last }
          val paramGet = param getOrElse { new java.util.Date } getTime

          //TODO: notes to self - don't say 'him', say 'you', etc.

          val msg = rawMsg.trim
          //TODO: possible bug if name and name++ are both present, and probably others ;)
          var nicks = rawNicks.split(",").map(_.replaceAll("[:.@]", "").trim).filter(_.nonEmpty).toSet
          var toPlusNicks = nicks.filter(_.endsWith("++")).map(_.replaceAll("[+]", ""))
          nicks = nicks.map(_.replaceAll("[+]", "")) 
          val isHere = if(param.isDefined) Set[String]() else nicks.filter(nick => users.map(_.toLowerCase).contains(nick.toLowerCase))
          val errNicks = nicks.filter(nick => !msgs.isKey(nick))
          val toMsgNicks = ((nicks &~ isHere) &~ errNicks)
          toPlusNicks = ((toPlusNicks &~ isHere) &~ errNicks)
          val dontMsgNicks = errNicks ++ isHere ++ (if(msg.isEmpty) (toMsgNicks &~ toPlusNicks) else Set())
          
          def themForm(nicks: Set[String]): String = if(nicks.size == 1) (if(nicks.head.isGirl) "her" else "him") else "them"
          def theyForm(nicks: Set[String]): String = if(nicks.size == 1) (if(nicks.head.isGirl) "she" else "he") else "they"
          def sForm(nicks: Set[String]): String = if(nicks.size == 1) "s" else "" // he find(s)
          def multiForm(nicks: Set[String]): String = if(nicks.size == 1) "" else "s" // boy(s)
          
          if(isHere.nonEmpty) {
            if((isHere contains this.name) || (isHere contains sender)) { //TODO: case bug
              speak(
                "wat.",
                "Oh, you...",
                if(sender.isBot) "Knock it off, bro!" else "Knock it off, meatbag!",
                Memes.it_was_you,
                Memes.NO_U)
            } else {
              speak((if(sender.isGirl) "woman, " else "dude, ")+isHere.mkString(", ")+(if(isHere.size == 1) " is " else " are ")+"right here...")
            }
          }
          if(errNicks.nonEmpty) {
            speak("no offence, but "+errNicks.mkString(", ")+(if(errNicks.size == 1) "doesn't sound like a real name to me." else "don't sound like real names to me."))
          }
          if(msg.isEmpty && (toMsgNicks &~ toPlusNicks).nonEmpty) { //TODO: test a++,b,c... also make sure if names are relevant
            speak("hmm... but what should I tell "+themForm(toMsgNicks &~ toPlusNicks)+"?")
          }
          if(toMsgNicks.nonEmpty) {
            var anyMsg = false
            for(nick <- toMsgNicks) {
              val (timeStamp,finalMsg) = (paramGet.toString, (if(toPlusNicks contains nick)"++ "else"") + msg)
              val toMsg = timeStamp + "," + finalMsg
              if(finalMsg.nonEmpty) {
                anyMsg = true
                msgs += (nick.toLowerCase, toMsg)
              }
            }
            
            if(anyMsg) {
              var say = List(
                c"{o}k{.|, chief}",
                c"it['ll| shall| will] [be|get] done{.}", 
                c"ay{-ay} {cap'n|captain}!", 
                c"{sure,|ok,|ok yeah,|ay,} I['ll| will]"+" "+Seq(
                  c"[tell|relay it to] ${themForm(toMsgNicks)}{.}",
                  c"make [sure|certain]"+" "+Seq(
                    c"${theyForm(toMsgNicks)} [get|recieve]${sForm(toMsgNicks)} this {msg|message}",
                    c"this {msg|message} reaches ${themForm(toMsgNicks)} {when ${theyForm(toMsgNicks)} return${sForm(toMsgNicks)}} {here}").random).random + c"{.}")
              
              if(dontMsgNicks.nonEmpty) {
                say = say.map(_ + " (" + 
                  c"{well, } [except|but not] for "+" "+Seq(
                    c"the [ppl|people|humans] I've just [complained about|mentioned]",
                    "the " +
                      (if(dontMsgNicks.size == 1) 
                        (if(dontMsgNicks.forall(_.isGirl)) c"[girl|woman|lady]" else c"[guy|man|gentleman]")
                      else
                        (if(dontMsgNicks.forall(_.isGirl)) c"[girls|women|ladies]" else if(dontMsgNicks.exists(_.isGirl)) c"[ppl|people|humans]" else c"[guise|guys|men|gentlemen]")
                      )+" "+c"I've just [complained about|mentioned]",
                      dontMsgNicks.init.mkString(", ")+(if(dontMsgNicks.size > 1) " and " else "")+dontMsgNicks.last
                    ).random + ")")
              }
              
              speak(say: _*)
            }
          }
        case _ =>
          speak(c"Sorry, but I {still|really|kind of|unfortunately} don't know what to do with this.")
      }
    } else if(message.startsWithAny("@reword ", "@rephrase ")) {
      val toReword = message.dropWhile(_ != ' ').tail
      var rephrased = wordnet.rephrase(toReword)
      def isRepost: Boolean = (rephrased == toReword || rephrased == lastMsg)

      var maxIters = 5
      while(maxIters > 0 && isRepost) {
        rephrased = wordnet.rephrase(toReword)
        maxIters -= 1
      }

      speak(if(isRepost) "Sorry, I've got nothing..." else rephrased)
    } else if(message.startsWithAny("@context", "@tldr", "@tl;dr", "@keyword")) spawn {
      val text: String = recentContext
      
      println(text)
        
      speakNow(
        zemanta.suggestKeywords(text, 3~4).emptyToNone.orElse(wordnet.keywords(text, 3~4).emptyToNone) map { keywords =>
          val keywordStr = keywords.mkString(", ").toLowerCase
          List(
            c"{I'd say|I think} it's about $keywordStr.",
            c"It {could|might} be about $keywordStr.")
        } getOrElse {
          List(
            "I have no idea"+"."*0~3,
            "I don't know what this is about"+"."*0~3)
        } :_*)
    } else if(message.startsWithAny("@suggest")) spawn {
      val cntReg = """@suggest[(]([1-5])[)].*""".r
      val cnt = message match {
        case cntReg(count) => count.toInt
        case _ => 2
      }
      
      val text: String = recentContext
      println(text)
      
      speakNow(
        zemanta.suggestArticles(text)
          .map(_.filterNot(article => URLs.contains(article.url)).take(cnt)).emptyToNone
          .map(articleList => for(article <- articleList) yield article.title + " " + bitly.tryShorten(article.url))
          .getOrElse(List("Sorry, I've got nothing...")):_*)
    } else if(message.contains("@all") && !(users.contains("botko") || users.contains("_botko_"))) {
      speak((users.toBuffer -- mustNotBeNamed).mkString(", "))
    } else if((message matches "@?"+name+"[:, ]{0,3}(uptime|updog)") 
      || (message.startsWithAny("uptime ", "@uptime") && mentions.nonEmpty)
      || (message.startsWithAny(users.toSeq: _*) && message.split(" ").size <= 5 && message.contains("uptime"))) {
      
      val mytime = withAlternative(getSinceString(startTime), "who knows how long..")
      
      for(nick <- mentions) {
        if(nick == this.name) {
          val servertime = withAlternative(
            getSinceZeroString(("cat /proc/uptime".!!).trim.takeWhile(_ != '.').toInt),
            "who knows how long..")

          speak(c"{Well, } I['ve| have] been [in here|up|running|up and running|going] for $mytime {already|so far}, but my server has been running for $servertime{.}")
        } else {
          if(joinTimes(nick.toLowerCase) == -1) {
            speak(
              c"$nick has been [up|in here|online] for at least as long as I have, [which|and that] is >$mytime{.}",
              c"$nick has been here {for} {even} longer than I have, and I've been here for $mytime{.}")
          } else {
            val nicktime = getSinceString(joinTimes(nick.toLowerCase))
            speak(
              c"$nick has been [up|in here|online] for $nicktime {already|so far}{.}",
              c"$nick is [up|in here|online] for $nicktime {already|so far}{.}")
          }
        }
      }
    } else if((message matches "@?"+name+"[:, ]{0,3}help.*") || message == "@help "+name) {
      speak(c"Sorry, {but} [I'm|I am] not [very|too] helpful{.}3")
    }
    
    // checks twitter only every few minutes, and only if people are talking on channel
    checkTwitter() 

    //last msgs
    lastMsgs = lastMsgs :+ message
    if(lastMsgs.size > 7) lastMsgs = lastMsgs.tail
  }
  
  val lastPrivateMessage = HashMap[String, String]().withDefaultValue("")
  def speakPriv(message: String, nick: String, msgs: String*) {
    import sys.process._
    val nickClean = nick.replaceAll("[^a-zA-Z]", "") // TODO: use cleanNick + clean for filename - make File wrapper that takes care of that, get filename regex, etc., close file
    appendToFile("logs/chat_"+nickClean+".log")(nickClean+" "+message)
  
    for(newMsg <- (msgs.toBuffer - lastPrivateMessage(nick)).randomOption) {
      Thread.sleep(500+nextInt(500*2))
      sendMessage(nick, newMsg)
      lastPrivateMessage(nick) = newMsg
      appendToFile("logs/chat_"+nickClean+".log")(name+" "+newMsg)
    }
  }

  override def onPrivateMessage(sender: String, login: String, hostname: String, message: String) {
    if(users contains sender) message.makeEasy.replaceAll(
      "i'm" -> "i am", 
      "i've" -> "i have", 
      "i'll" -> "i will", 
      "i'd" -> "i would",
      "i was" -> "i am", //TODO - seems to work
      "gonna" -> "going to", 
      "they're" -> "they are", 
      "we're" -> "we are",
      "don't" -> "dont"
    ).split(" ").toList match {
      case List("hello") | List("hi") => speakPriv(message, sender,
          "How do you do?",
          "Hi. How are you?")
      case "i" :: "am" :: x => 
        if(x.size > 0 && x(0).endsWith("ing")) { // doING something
          val x2 = x.map(word => if(List("my","mine") contains word) "your" else word)
          //Memory <= (IsVerbing, x2.mkString(" "))
          speakPriv(message, sender,
            "How does "+x2.mkString(" ")+" make you feel?",
            "How long have you been "+x2.mkString(" ")+"?")
        } else if(x.size > 0 && List("a","an","the").contains(x(0))) { // being A something
          //Memory <= (IsNoun, x.mkString(" "))
          speakPriv(message, sender,
            "How long have you been "+x.mkString(" ")+"?",
            "How does being "+x.mkString(" ")+" make you feel?")
        } else if(x.size == 1) {
          //Memory <= (IsNoun, x.mkString(" "))
          speakPriv(message, sender,
            "How long have you been "+x.mkString(" ")+"?")
        } else {
          speakPriv(message, sender,
            "How does that make you feel?",
            "How long have you been "+x.mkString(" ")+"?")
        }
      case "i" :: "feel" :: "like" :: x => 
          if(x.size > 0 && x(0) == "my") 
            speakPriv(message, sender,
              "Why do you think your "+x.tail.mkString(" ")+"?")
          else 
            speakPriv(message, sender,
              "What makes you think that?",
              "Why do you think that is?")
      case "i" :: "feel" :: x => 
          speakPriv(message, sender,
            "How long have you been feeling"+x.mkString(" ")+"?",
            if(x.size > 1) "Does anyone else you know "+x(0)+" "+x.tail.mkString(" ")+"?" else
            "Why do you feel that way?")
      case "i" :: "dont" :: x => 
        speakPriv(message, sender,
          "Why don't you "+x.mkString(" ")+"?")
      case "i" :: "would" :: x => 
        speakPriv(message, sender,
          "Why don't you?")
      case "i" :: verb :: x =>
        speakPriv(message, sender,
          "Tell me more about "+x.mkString(" ")+".",
          "Does anyone else you know "+verb+" "+x.mkString(" ")+"?")
      case w1 :: "you" :: x :: "me" :: _ => 
        speakPriv(message, sender,
          "What makes you think I "+x+" you?",
          "Why do you think I "+x+" you?")
      case w1 :: w2 :: "you" :: x :: "me" :: _ => 
        speakPriv(message, sender,
          "What makes you think I "+x+" you?")
      case "they" :: "are" :: x :: _ => 
        speakPriv(message, sender,
          "Why do you think they're "+x+"?")
      case "because" :: _ => 
        //val mem = Memory->(IsVerbing)
        speakPriv(message, sender,
          //if(mem.isDefined) {
          //  "OK. would you like to talk about "+mem.get+"?"
          //} else
          "I understand... would you like to talk about something else?",
          "OK... but how does that make you feel?")
      case "since" :: x => 
        speakPriv(message, sender,
          "What did you do before that?",
          "Are you sure it wasn't "+Seq("sooner", "later").random+"?")
      case "for" :: "instance" :: x => 
          if(x.size > 1) 
            speakPriv(message, sender, "Can you think of any other examples?") //for instance, bla bla bla
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
              })
      case "for" :: x => 
        speakPriv(message, sender,
          if(x.contains("years")||x.contains("long")||x.contains("while")) 
            "What can you recall from before that?"
          else 
            "What did you think before that?")
      case "yes" :: x => 
        speakPriv(message, sender,
          "You seem sure...",
          "Are you certain?",
          "Are you sure?")
      case "no" :: x => 
        speakPriv(message, sender,
          "Why not?")
      case x => 
        if(x.contains("you")) 
          speakPriv(message, sender,
            "Lets talk about something else...",
            "Do you really think that about me?") //TODO
        else {
          //val memNoun = Memory->(IsNoun)
          //val memVerb = Memory->(IsVerbing)
          speakPriv(message, sender,
            //if(memNoun.isDefined) {
            //  speak(
            //    "Let's talk more about you being "+memNoun.get+".")
            //} else
            c"{Perhaps|Maybe} {you're right}",
            "Let's change the topic"+"."*0~3,
            //if(memVerb.isDefined) {
            //  speak(
            //    "Let's talk about "+memVerb.get+" some more.")
            //} else
            "You think so?",
            "Why is that?",
            "Please tell me more.")
        }
    }
  }
}

