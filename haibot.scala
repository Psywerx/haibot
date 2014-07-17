package org.psywerx

import org.psywerx.util._
import org.psywerx.Time._
import org.psywerx.Caption.C

import org.jibble.pircbot._
import collection.mutable
import scala.util.Random._
import java.io._
import scala.concurrent._
import ExecutionContext.Implicits.global
import math._

final object haibot { def main(args: Array[String]): Unit = new haibot }

final class haibot extends PircBot {
  var startTime = now
  val joinTimes = mutable.AnyRefMap[String, Int]() withDefaultValue -1
  
  val config = Store(".config").toMap
  val (folder, login, name, chan, serv, ssl, port, pass, owner) = (
    config("folder"),
    config("login"),
    config("name"),
    config("chan"),
    config("serv"),
    config("ssl", "false").toBoolean,
    config("port", "6667").toInt,
    config("pass", null),
    config("owner")) //owner prefix actually
  
  this.setVerbose(true)
  this.setLogin(login)
  this.setName(name)
  locally {
    var backoff = 10000L //ms
    var connected = false
    do {
      try {
        // pircbot
        //this.connect(serv, port.toInt, pass)
        // pircbot-ssl
        val sslFactory = if(ssl) new TrustingSSLSocketFactory else null
        this.connect(serv, port, pass, sslFactory)
        connected = true
      } catch {
        case e: Exception => //TODO: this "reconnect" is untested
          this.disconnect()
          Thread.sleep(backoff)
          backoff = min(backoff+1000, 200000)
          e.printStackTrace
          println("Retrying in "+backoff/1000+"s ...")
      }
    } while(!connected)
  }
  this.joinChannel(chan)

  val apiKeys = Store(".apikeys").toMap

  lazy val wordnet = new WordNet("lib/WordNet/")
  lazy val zemanta = new Zemanta(apiKeys("Zemanta"))
  lazy val bitly   = new Bitly(apiKeys("bitly1"), apiKeys("bitly2"))
  lazy val novatel = new NovatelSMS(apiKeys("novatel_user"), apiKeys("novatel_pass"))
  val smsCoolDown = 77 //seconds
  var lastSMSTime = 0

  val events = Store(folder+"events.db")

  //TODO: cache them at least for a minute or so
  def sheSaid        = getFile(folder+"twss.db",          allowFail = true)
  def awwwBag        = getFile(folder+"awww.db",          allowFail = true).toSet
  def noawwwBag      = getFile(folder+"noawww.db",        allowFail = true).toSet
  def mehBag         = getFile(folder+"meh.db",           allowFail = true).toSet
  def nomehBag       = getFile(folder+"nomeh.db",         allowFail = true).toSet
  def mustNotBeNamed = getFile(folder+"dontmention.db",   allowFail = true).toSet
  def noOnlineOnMsg  = getFile(folder+"noonlineonmsg.db", allowFail = true).map(_.cleanNick).toSet
  def females        = getFile(folder+"females.db",       allowFail = true).map(_.cleanNick).toSet
  def males          = getFile(folder+"males.db",         allowFail = true).map(_.cleanNick).toSet
  def trusted        = getFile(folder+"trusted.db",       allowFail = true).map(_.cleanNick).toSet
  def bots           = getFile(folder+"bots.db",          allowFail = true).map(_.cleanNick).toSet
  val untrusted      = Store(folder+"untrusted.db")
  val seen           = Store(folder+"seen.db")
  
  implicit class IRCNickString(val s: String) {
    def cleanNick: String = s.toLowerCase.replaceAll("[0-9_^]|-?(nexus|work|home|(an)?droid|i?phone)$", "")
    def isFemale: Boolean = females.contains(s.cleanNick)
    def isMale: Boolean = males.contains(s.cleanNick)
    def isTrusted: Boolean = 
        ((trusted.contains(s.cleanNick))
      && (tempDontTrustSet(s.cleanNick) == 0)
      && (!untrusted.contains(s.cleanNick)))
    def isBot: Boolean = (s.startsWith("_") && s.endsWith("_")) || bots.contains(s.cleanNick)
  }
  
  val tempDontTrustSet = mutable.AnyRefMap.empty[String, Long] withDefaultValue 0
  val trustTimeOut = 15000 //ms
  def tempDontTrust(nick: String): Unit = thread {
    tempDontTrustSet.synchronized {
      val trustLvl = max(tempDontTrustSet(nick), tempDontTrustSet(nick.cleanNick)) + 1
      tempDontTrustSet(nick)           = trustLvl
      tempDontTrustSet(nick.cleanNick) = trustLvl
      if(trustLvl >= 4 && !untrusted.contains(nick.cleanNick)) {
        untrusted += nick.cleanNick
      }
    }
    Thread.sleep(trustTimeOut)
    tempDontTrustSet.synchronized {
      val trustLvl = max(tempDontTrustSet(nick), tempDontTrustSet(nick.cleanNick)) - 1
      tempDontTrustSet(nick)           = trustLvl
      tempDontTrustSet(nick.cleanNick) = trustLvl
    }
  }
  
  var lastMsgs = List[String]()
  
  var twitterCheck = 0
  val twitterCheckInterval = 5*60
  def checkTwitter(force: Boolean = false): Unit = synchronized {
    if(since(twitterCheck) > twitterCheckInterval || force) {
      try {
        import sys.process._
        val mentions = Seq("t", "mentions", "-n", "5").!!.trim
        if(mentions(0) == '@') {
          val lastTweets = getFile(folder+"lasttweets.db")
          val mentionList = mentions.replace("\n   ", " ").split("\n").take(5).map(_.trim).takeWhile(tw => !(lastTweets contains tw))
          if(mentionList.nonEmpty) {
            // Save the last few mentions
            val newLastTweets = (mentionList ++ lastTweets).take(10)
            printToFile(folder+"lasttweets.db")(newLastTweets.mkString("\n"))

            // Speak the new mentions
            for(mention <- mentionList) {
              val (name, msg) = mention.splitAt(mention.indexOf(' '))
              speak(name.drop(1) + " on Twitter says:" + msg)
            }
          }
        }
      } catch {
        case e: Exception => 
          e.printStackTrace
      } finally {
        twitterCheck = now
      }
    }
  }
  
  //TODO: this is horrible and error-prone, make class
  var tweetScore    = Set.empty[String]
  var tweetPlsScore = Set.empty[String]
  var tweetNegScore = Set.empty[String]
  var tweetNames    = Set.empty[String]
  var tweetMsg = ""
  var tweetId = ""
  var tweetLim = 3

  var lastMsg_ = ""
  def speakNow(msgs: String*): Unit = {
    if(msgs.nonEmpty) {
      val newMsg = (msgs.toBuffer - lastMsg_).randomOption.getOrElse(lastMsg_)
      sendMessage(chan, newMsg)
      lastMsg_ = newMsg
    }
  }
  def speak(msgs: String*): Unit = {
    Thread.sleep((777 + nextInt(777*2)).toLong)
    speakNow(msgs: _*)
  }
  val messages = Store(folder+"msgs.db")
  // Fetch messages for nick and speak them
  def speakMessages(nick: String, spoke: Boolean, joined: Boolean): Unit = synchronized {
    val now = (new java.util.Date).getTime //TODO: put into lib
    var hadMsgs = false
    var allMsgs = messages.toList
    val msgs = allMsgs.filter { _._1 == nick.toLowerCase }
    for(row @ (_nick, rawMsg) <- msgs) {
      val (param, msg) = 
        (rawMsg splitAt (rawMsg indexOf ',')) match { case (s1, s2)  => (s1, s2.tail) }
      
      val params = param.split("[|]").toSet
      val timeParams = withAlternative(params.filter { _ matches "[0-9]+" }.map{ _.toLong }, Set.empty)
      
      if((params("onspeak") && spoke)
      || (params("onjoin") && joined)
      || (timeParams exists { _.toLong <= now })) {
        hadMsgs = true
        allMsgs = allMsgs.filterNot { _ == row }
        if(msg.startsWithAny("++", nick+"++"))
          speak(nick+"++" + msg.dropWhile(_ != '+').drop(2))
        else
          speak(nick+": "+msg)
      }
    }
    if(hadMsgs) messages replaceWith allMsgs
  }

  var shutdown = false
  override def onDisconnect(): Unit = {
    println("onDisconnect: event triggered.")
    var backoff = 5000L //ms
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
      backoff = min(backoff+5000, 60000)
      Thread.sleep(backoff)
    }
  }

  def getUsers: Set[String] = {
    val users = super.getUsers(chan)
    if(users == null) Set.empty[String]
    else users.map(_.getNick).toSet
  }
  
  override def onNickChange(oldNick: String, login: String, hostname: String, newNick: String): Unit = {
    tempDontTrust(oldNick)
    tempDontTrust(newNick)
    if(!seen.contains(newNick.cleanNick)) {
      seen += newNick.cleanNick
    }
    speakMessages(newNick, spoke = false, joined = true)
  }
  
  override def onJoin(channel: String, sender: String, login: String, hostname: String): Unit = {
    if(sender == this.name) {
      startTime = now
    } else {
      joinTimes(sender.toLowerCase) = now
      if(sender.startsWith(owner) && 0.05.prob)
        speak( //TODO: detect netsplit
          c"welcome, father{!|.}",
          c"welcome back{!|.}",
          c"hi, you{!}",
          c"I've missed you, $sender{.}")
      
      if(!seen.contains(sender.cleanNick)) {
        seen += sender.cleanNick
        if(0.2.prob) speak(
          c"welcome to #psywerx, $sender",
          c"oh cool, new people... or at least new enough that I don't recognize them")
      }

      speakMessages(sender, spoke = false, joined = true)
    }
  }
  
  override def onUserList(channel: String, users: Array[User]): Unit = {
    val users = getUsers
    if(users.size > 1) speak("o hai!", if(users.size == 2) "hi, you!" else "hai guise!", "ohai", c"hello{!}", "hi", "hi there!")
    for(user <- users) {
      if(!seen.contains(user.cleanNick)) {
        seen += user.cleanNick
      }
      speakMessages(user, spoke = false, joined = true)
    }
  }

  override def onMessage(channel: String, sender: String, login: String, hostname: String, message: String): Unit = {
    speakMessages(sender, spoke = true, joined = false)
    val msg = message.makeEasy
    val msgBag = msg.split(" ").toSet
    lazy val sentences = message.sentences
    val users = getUsers
    //TODO: @nick nick. <- get the proper regex here
    val mentions = message.replaceAll("[@,:]", " ").split(" ").toSet & (users ++ users.map(_.toLowerCase) ++ (users.map(_.toLowerCase) & bots).map(_.replace("_", "")))
    val URLs = message.findAll(Regex.URL).distinct
    lazy val URLsText = Net.scrapeURLs(URLs: _*)
    lazy val URLsWords = 
      URLsText
        .replaceAll("[^a-zA-Z0-9 .'/-]", " ") //notsure if I should have this one here
        .split("\\s")
        .filter(_.length.isBetween(3, 34))///"Supercalifragilisticexpialidocious".length
    lazy val recentContext = 
      (URLsText + ". " + lastMsgs.mkString(". ") + ". " + message)
        .replaceAll(Regex.URL.toString, "")
        .replaceAll("@[a-z]+[ ,:]", "")
        .replaceAll("^[ .]+", "")
        .trim
    
      val (yes, no, maybe, meh, please, quit) = (
          message.startsWithAny("@yes", "@yep", "@yea", "@sure", "@suer"),
          message.startsWithAny("@no", "@nein", "@nah", "@nay"),
          message.startsWithAny("@maybe", "@perhaps"),
          message.startsWithAny("@meh", "@whatever"),
          message.startsWithAny("@please"),
          message.startsWithAny("@leave "+name, "@quit "+name, "@gtfo "+name, "@die "+name))

    // Oh look, AI
    if(sender.startsWith(owner) && quit) {
      speakNow(if(users.size > 2) "bai guise!" else "good bye, you.", "buh-bye!", "au revoir!")
      shutdown = true
      sys.exit(0)
    } else if(msg.startsWithAny("hai ", "ohai ", "o hai ", "hi ", "'ello ", "ello ", "oh hai", "hello") && (0.35.prob || (0.9.prob && mentions.contains(name)))) {
      var hai = Seq("ohai", "o hai", c"[hello|hi] {there}")
      if(mentions.contains(name) && 0.7.prob) {
        hai = hai.map(_ + " " + sender)
      } else if(mentions.nonEmpty && !mentions.contains(name) && 0.7.prob) {
        hai = hai.map(_ + " " + mentions.toSeq.random)
      }
      
      hai = hai.map(_ + " " + c"{:)|!|^_^}").map(_.replaceAll(" !", "!").trim) //first new caption system fail :/
      
      speak(hai: _*)
    } else if(message.contains(name+"++") && 0.65.prob) {
      speak(
        c"woo{o}hoo{o}!",
        "yeah!",
        """\o/""",
        "scoar!",
        if(0.2.prob) s"$sender++" else "yaaay!")
    } else if((msg.startsWithAny("yes "+name, "exactly "+name) && 0.65.prob)
           ||((msg.startsWithAny("yes ", "exactly ") && mentions.contains(name) && 0.3.prob))) {
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
    } else if(message.containsAny("0-9", "a-z", "A-Z", ")*", "]*", ".*", ".+")
           && message.indexOf('[') < message.indexOf(']')
           && (if(message.containsAny("^", "$", "{", "}")) 0.7 else 0.5).prob) {
      speak(
        c"ooo{o}h, is that a regex? I [<3|love] regexes[!]2",
        c"Regex{es}[!]2 {M|m}y favorite thing[!]2",
        c"mm{m}{m}, regex{es}{!|...}",
        c"{wow, }I wonder what that matches.{.}")
    } else if((msg.containsAny("tnx", "thanks", "hvala"))
           &&((mentions.contains(name) && 0.8.prob) || (msg.contains(" alot ") && 0.5.prob))) {
      if(msg.matches(".* alot([.,?! ]|$)") && !msg.contains("hvala")&& 0.9.prob) {
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
    } else if(msg.startsWithAny("fucking ", "fakin") && sentences(0).split(" ").size.isBetween(2, 5) && 0.75.prob) { 
      speak(
        "how does "+sentences(0).trim+" feel?",
        "having sex with "+sentences(0).substring(sentences(0).indexOf(' ')+1).trim+"?",
        "come on, "+sender+"... don't fuck "+sentences(0).substring(message.indexOf(' ')+1).trim)
    } else if(msg.containsAny("but sex", "but fuck") && 0.7.prob) { 
      speak(c"[has someone mentioned|did someone mention] butt sex?")
    } else if(msg.containsAny("shutup", "shut up", "fuck you", "damn", "stfu", "jebi se")
           &&((mentions.contains(name) && 0.9.prob) || ((mentions & bots).nonEmpty && 0.8.prob))) {
      speak(
        c"U MAD, BRO?{ :P}",
        Memes.NO_U,
        "NO U!",
        c"This wouldn't happen if you made us better{.}1-3",
        c"Yeah, blame it on the bots{.}1-3")
    } else if(msg.containsAny(sheSaid: _*) && 0.66.prob) {
      speak("That's what she said!")
      
    // ex meh_bot
    } else if((((msgBag & mehBag).size-1)*0.15 - (msgBag & nomehBag).size*0.33).prob) {
      speak("meh.")
    
    // ex aww_bot
    } else if(URLs.nonEmpty) thread {
      val words = URLsWords.map(_.toLowerCase).toSet
       
      if((((awwwBag & words).size - (noawwwBag & words).size)*0.2).prob) {
        speak(
          c"{d}[a]3[w]3-5{!}2",
          c"{lol, }{how }cute{!} [:)|^_^]",
          c"so{.} cute.",
          if((words & Set("fluff", "puff")).nonEmpty) Memes.so_fluffy else "aww!")
      }
    } else if(msg.containsAny("i jasn","wat"," how","how", "kako","ne vem","krneki")
           &&(if(message contains "?") 0.6 else 0.4).prob) {
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
      } else if(msg.containsAny(" ljud", " folk", "people") && 0.5.prob) {
        speak(c"{yeah,} people are [weird|strange]{, I guess}...")
      } else if((mentions contains this.name) && 0.9.prob) {
        speak(c"I can't tell if you're asking me, or asking about me.")
      } else if(0.27.prob || min(0.5, message.count(_ == '?') * 0.15).prob) {
        speak(
          c"[I am|I'm] [confused|not sure|asking myself] about this[ also|, too]{.}3",
          c"This [puzzles|confuses] me {greatly|very much}[ also|, too]{.}3",
          c"[I don't know|I dunno|I have no idea|No idea]... hope [I've helped|this helps] {at least a little|in some way}{.}3",
          c"[I wouldn't|Don't] worry {about it|about that} {so much|too much}, I'm sure you'll figure it out {or something} {eventually|with time|in time}{.}3",
          c"I guess {you could say|one might say} that is some[thing|what| kind] of a [conundrum|mystery]{.}3",
          (if(mentions.nonEmpty && 0.8.prob)
             c"[I don't know|I have no idea][, |...] but {yes, }${mentions.toSeq.random} might."
           else
             c"[Have you tried|Did you try|Have you attempted|Did you attempt|Tried] [searching the|looking on the|querying the|inquiring upon the|checking the|making use of the] [internet|intarweb|webosphere|DERPAnet|ARPAnet|cyberspace|electronic noosphere|information super-highway|W3 Infobahn]{s}?"
          ))
      }
    }

    
    if(message.startsWithAny("@#", "#@")) {
      // Ignore
    } else if(message.startsWithAny("@ocr ", "@read ")) {
      val imgurReg = """(https?://)(?:www[.])?(imgur.com/)(?:(?:gallery/)|(?:r/[a-z]+/))?([A-Za-z0-9]+)""".r
      def toImgurImg(url: String): String = url match {
        case imgurReg(protocol, domain, img) => (if(this.ssl) "https://" else protocol)+"i."+domain+img+".png"
        case _ => url
      }
      val pics = URLs.map(toImgurImg).filter(_.endsWithAny(".jpg", ".jpeg", ".png", ".gif", ".jpeg:large", ".jpg:large"))
      if(pics.size < URLs.size) speakNow(c"{Sorry,} I only read a few select formats.")
      
      for(pic <- pics) Net.withDownload(pic) {
        case Some(tempFile) =>
          speakNow(
            OCR.OCR(tempFile)
              .map(guess => List(s"I think it says: $guess", s"My best guess is: $guess"))
              .getOrElse(List(c"Sorry, {I have} no idea, but I{'m| am} still learning {how} to read.")):_*)
        case _ =>
          speak(c"I don't {even} know how to download this{ file}.")
      }
    } else if(message.startsWithAny("@shorten ", "@bitly ")) {
      if(URLs.nonEmpty) {
        val bitlyURLs = URLs.flatMap(bitly.shorten).mkString(" ")
        if(bitlyURLs.isEmpty) {
          speak("Sorry, couldn't shorten url"+(if(URLs.size > 1) "s" else "")+"...")
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
      //TODO: how do I find the title... boilerpipe? http://metabroadcast.com/blog/boilerpipe-or-how-to-extract-information-from-web-pages-with-minimal-fuss
      val title = 
        message
          //.replaceAll(Regex.Date.toString, "")
          .replaceAll(Regex.URL.toString, "")
          .substring("@event ".length).trim
        
      val missing = mutable.ListBuffer[String]()
      if(title.isEmpty) missing += "title"
      if(dates.isEmpty) missing += "date"
      if(URLs.isEmpty)  missing += "URL"
      
      if(missing.nonEmpty) {
        val it_them = if(missing.size == 1) "it" else "them"
        speak("I don't have "+missing.mkString(", ")+s" for this. Paste ${it_them} in text form, pls.")
      } else {
        speak("Is this right? "+dates(0)+" -- "+title)
        speak("Yeah, looks OK, but I'm not gonna save it anywhere or anything :)")
        //events + (dates(0).replaceAll("[/. ]","_"), title+" "+URLs.mkString(" "))
      }
    } else if(message.startsWith("@event")) {
     for(event <- events.*) speak(event._1 + (if(event._2 != null) " "+event._2 else ""))
    } else if(message.startsWith("-event ")) {
      val rem = events ?- message.substring("-event ".length) //TODO: lolwat
      speak("I have removed "+rem.length+" event"+(if(rem.length != 1) "s" else ""))
    // Twitter/FB part
    } else if(yes || maybe || please) {
      val beggedBefore = (tweetPlsScore contains sender.cleanNick)
          
      if(!sender.isTrusted) tempDontTrust(sender)
      
      if(sender.isTrusted && (yes || (maybe && 0.5.prob)))
        tweetScore += sender.cleanNick
      
      if(please) {
        if(beggedBefore && 0.4.prob)
          speak("Come on "+sender+", stop begging", "You may beg only once, "+sender+".")

        if(!beggedBefore && sender.isTrusted)
          tweetPlsScore += sender.cleanNick
      }
      
      import sys.process._
      def isOverTweetLimit: Boolean = (tweetScore.size-tweetNegScore.size >= tweetLim) || (please && !beggedBefore && 0.25.prob)
      if(tweetMsg == null && tweetId == null && tweetNames.nonEmpty) {
        val twitterReturn = (Seq("t", "follow") ++ tweetNames).!
        if(twitterReturn == 0)
          speak("Follow'd!", "It's done", "It is done.")
        else
          speak("Failed to follow :/")
      
        tweetScore    = Set.empty
        tweetNegScore = Set.empty
        tweetPlsScore = Set.empty
        tweetNames    = Set.empty
      } else if(tweetMsg == null && (tweetId matches "[0-9]*") && isOverTweetLimit) {
        val returnTwitter = Seq("t", "retweet", tweetId).!
        
        var successResponses = List("Retweeted it!", "It's done", "It is done.", "I retweeted the tweet out of that tweet.")

        // Post to Facebook too - get tweet details from twitter
        import sys.process._
        val tweetDetails = withAlternative(
          (Seq("t", "status", tweetId).!!)
            .split("\n")
            .map(line => line.splitAt("Screen name  ".length))
            .map(line => (line._1.trim, line._2))
            .toMap,
          Map.empty[String, String]
        ).withDefaultValue("")
        
        if(tweetDetails("Text") != "") {
          // fbcmd 1.1
          // val facebookReturn = Seq("fbcmd", "PPOST", apiKeys("facebookpage"), ...
          // fbcmd 2.x
          val facebookReturn = 
            Seq("fbcmd", "AS", apiKeys("facebookpage"), "POST",
              "",                                      // Post Message
              " "+tweetDetails("Screen name"),         // Post Name
              "https://twitter.com/statuses/"+tweetId, // Post Link
              "via Twitter",                           // Post Caption
              " "+tweetDetails("Text")).!              // Post Description
          
          val screenName = tweetDetails("Screen name")
          if(screenName.nonEmpty) {
            successResponses ++= List(
              c"""[I've r|R|r]etweeted {the tweet out of} [that|the] {lovely|nice} tweet by $screenName{!}""",
              c"I[ hope|'m hoping]"+s" $screenName is "+c"""[pleased with|happy for|happy about] this retweet{.}""")
          }
        }
        
        if(returnTwitter == 0)
          speak(successResponses: _*)
        else
          speak("Failed to retweet :/")

        tweetScore    = Set.empty
        tweetNegScore = Set.empty
        tweetPlsScore = Set.empty
        tweetMsg = ""
      } else if(tweetMsg != null && tweetMsg.nonEmpty && tweetMsg.size <= 140 && isOverTweetLimit) {
        val twitterReturn  = Seq("t", "update", tweetMsg).!
        val facebookReturn = Seq("fbcmd", "AS", apiKeys("facebookpage"), "POST", tweetMsg).!
        
        (twitterReturn, facebookReturn) match {
          case (0, 0) => speak(c"It['s| is| has been] [posted|done]{.}", c"I['ve| have] done as you requested", c"I{'ve} tweeted it{, and facebook'd it}{!}", "Posted it.")
          case (0, _) => speak("Tweeted it, but facebooking failed!")
          case (_, 0) => speak("Facebook'd it, but tweeting failed!")
          case (_, _) => speak("Failed to post anywhere :(")
        }

        tweetScore    = Set.empty
        tweetNegScore = Set.empty
        tweetPlsScore = Set.empty
        tweetMsg = ""
      }
    } else if(no || (meh && 0.5.prob)) {
      tweetNegScore += sender.cleanNick
    } else if(message.startsWith("@checktweets") && sender.isTrusted) {
      checkTwitter(force = true)
    } else if(message.startsWith("@follow ")) {
      val names = 
        message
          .drop("@follow ".length)
          .trim
          .replaceAll("(https?[:]//)?(www.)?twitter.com/", "")
          .replace("@", "")
          .split(" ")
          .toSet
        
      if(names forall { _ matches "[A-Za-z0-9_]{1,20}" }) {
        tweetMsg = null
        tweetId = null
        tweetNames    = names
        tweetScore    = Set(sender.cleanNick)
        tweetNegScore = Set.empty
        tweetPlsScore = Set.empty
        tweetLim = if(sender.isTrusted) 2 else 3
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
      tweetScore    = Set(sender.cleanNick)
      tweetNegScore = Set.empty
      tweetPlsScore = Set.empty
      tweetLim = if(sender.isTrusted) 2 else 3
      speak(
        c"A retweet of [this|that|the] tweet {, perhaps|, maybe}?",
        c"[Want me to|Should I|Am I to|Is it OK if I|Is it OK to|May I|Can I] retweet [this|that] {tweet}?",
        c"I [can|could] {totally} retweet [this|that], {but only} if you [guise|guys|ppl|people] [confirm it|want me to|agree]{.}3",
        c"{Hey, that} looks like a tweet... [should I|may I|do you want me to|want me to|would you like me to|would it be cool if I] retweet it?",
        c"If [one of you|someone] confirms [this|it], I'll retweet {it}{.}",
        c"Someone {please} confirm {this|this tweet}, and I'll {be happy to} retweet it{.}")
    } else if(message.startsWith("@world ")) {
      val tweet = {
        var out = message.drop("@world ".length).trim
        if(URLs.nonEmpty && out.size > 140) {
          var shortTweet = out
          for(url <- URLs) {
            val bitlyUrl = bitly.shorten(url)
            if(bitlyUrl.isDefined) shortTweet = shortTweet.replace(url, bitlyUrl.get)
          }
          println(s"Shortened tweet from ${out.size} to ${shortTweet.size}")
          out = shortTweet
        }
        
        out
      }
      
      if(tweet.size >= 1 && tweet.size <= 140) {
        val andGals = if((females & users).nonEmpty && nextBoolean) c"{and gals}" else ""
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
        tweetScore    = Set(sender.cleanNick)
        tweetNegScore = Set.empty
        tweetPlsScore = Set.empty
        tweetLim = if(sender.isTrusted) 2 else 3
      } else {
        speak("That's too long to tweet, you twit! ("+tweet.size+" char)")
      }
    }
    
    if((message startsWith "@smsreset") && (sender startsWith owner)) {
      lastSMSTime = 0
    } else if(message startsWith "@sms ") {
      val msg = message.drop("@sms ".length)
      val (name, sms) = msg.splitAt(msg.indexOf(' '))
      val nums = Store(folder+"numbers.db").toMap
      if(!sender.isTrusted) {
        speak(c"{Sorry, }I don't trust you{ (yet?)}.")
      } else if(name.trim.isEmpty || sms.trim.isEmpty) {
        speak(c"You['ve forgotten| forgot] the name or {possibly} the message.")
      } else if(nums contains name.toLowerCase) {
        if(since(lastSMSTime) < smsCoolDown) {
          speak(c"Wait {a bit|somewhat} longer before sending [another|the next] [message|sms|msg]{, please}{.}")
          lastSMSTime = now
        } else thread {
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
            speak(c"{Hmm, }[weird|strange]... Novatel {server} says: $response")
          }
        }
      } else {
        def theirForm(nick: String): String = if(nick.isFemale) "her" else if(nick.isMale) "his" else "their"
        speak(c"I don't have ${theirForm(name)} number.{.. and no, you can't tell me it :P} {Sorry about this}")
      }
    }
    
    /// @msg
    val msgReg = """ *@(msg|tell|ask|onmsg|onspeak|onjoin)(?:[(]([^)]*)[)])? ([-+a-zA-Z0-9_,^]*):? ?(.*?)""".r
    if(message matches msgReg.toString) {
      message match {
        case msgReg(command, rawParam, rawNicks, rawMsg) =>
          
          //TODO: sms
          val defaultParam = "onspeak|onjoin"
          def getParam(default: String = defaultParam): String = {
            val commandLower = command.toLowerCase.replace("onmsg", "onspeak")
            val rawParamLower = withAlternative(rawParam.toLowerCase.replace("onmsg", "onspeak"), null)
            
            if((commandLower == "onspeak")
            || (commandLower == "onjoin")) commandLower
            else if(rawParamLower != null) {
              if((rawParamLower == "onspeak")
              || (rawParamLower == "onjoin")
              || (rawParamLower.split("[|]").toSet == Set("onjoin", "onspeak"))) rawParamLower
              else Time.getFutureDates(rawParam).lastOption.map(_.getTime.toString) getOrElse default
            } else default
          }
          val param = getParam()

          val msg = rawMsg.trim
          //TODO: possible bug if name and name++ are both present, and probably others ;)
          var nicks = rawNicks.split(",").map(_.replaceAll("[:.@]", "").trim).filter(_.nonEmpty).toSet
          var toPlusNicks = nicks.filter(_.endsWith("++")).map(_.replace("+", ""))
          nicks = nicks.map(_.replace("+", ""))
          val isHere = nicks.filter(nick => users.map(_.toLowerCase).contains(nick.toLowerCase))
          val errNicks = nicks.filter(nick => !messages.isValidKey(nick))
          val noOnlineOnMsgNicks = isHere.filter(here => noOnlineOnMsg.contains(here.cleanNick))
          val neverSeen = nicks.filterNot(nick => seen.contains(nick.cleanNick))
          val watNicks = (if(!(param matches "[0-9]+") && (isHere contains this.name) || (isHere contains sender)) (Set(this.name, sender) & isHere) else Set.empty[String])
          val toMsgNicks = ((((nicks &~ errNicks) &~ watNicks) &~ noOnlineOnMsgNicks) &~ neverSeen)
          toPlusNicks = (toPlusNicks &~ errNicks)
          val dontMsgNicks = errNicks ++ (if(msg.isEmpty) (toMsgNicks &~ toPlusNicks) else Set.empty) ++ watNicks ++ noOnlineOnMsgNicks ++ neverSeen
          
          def themForm(nicks: Set[String]): String = if(nicks.size == 1) (if(nicks.head.isFemale) "her" else if(nicks.head.isMale) "him" else "them") else "them"
          def theyForm(nicks: Set[String]): String = if(nicks.size == 1) (if(nicks.head.isFemale) "she" else if(nicks.head.isMale) "he" else "they") else "they"
          def sForm(nicks: Set[String]): String = if(nicks.size == 1) "s" else "" // find(s)
          //def multiForm(nicks: Set[String]): String = if(nicks.size == 1) "" else "s" // guy(s)
          
          if(watNicks.nonEmpty) {
            speak(
              "wat.",
              "Oh, you...",
              Memes.it_was_you,
              Memes.NO_U)
          }
          if(errNicks.nonEmpty) {
            speak("no offence, but "+errNicks.mkString(", ")+(if(errNicks.size == 1) "doesn't sound like a real name to me." else "don't sound like real names to me."))
          }
          if(neverSeen.nonEmpty) {
            speak(c"[Probably|Possibly|Perhaps] [some|a] [typo|misspelling], {be}cause [I've never|I haven't] [seen|heard of] "+" "+neverSeen.mkString(", ")+" "+c"[a|']round [here|these parts].")
          }
          if(msg.isEmpty && (toMsgNicks &~ toPlusNicks).nonEmpty) { //TODO: test a++,b,c... also make sure if names are relevant
            speak("hmm... but what should I tell "+themForm(toMsgNicks &~ toPlusNicks)+"?")
          }
          if(noOnlineOnMsgNicks.nonEmpty) {
            if(noOnlineOnMsgNicks.size > 1) {
              val nicks = noOnlineOnMsgNicks.init.mkString(", ") + ", and " + noOnlineOnMsgNicks.last
              speak(s"""${nicks} don't do online msgs.""")
            } else {
              val nick = noOnlineOnMsgNicks.head
              speak(s"""${nick} doesn't do online msgs.""")
            }
          }
          if(toMsgNicks.nonEmpty) {
            var anyMsg = false
            for(nick <- toMsgNicks) {
              val (finalParam, finalMsg) = (param, (if(toPlusNicks contains nick) "++ " else "") + msg)
              val toMsg = finalParam + "," + finalMsg
              if(finalMsg.nonEmpty) {
                anyMsg = true
                messages += (nick.toLowerCase, toMsg)
              }
            }
            
            if(anyMsg) {
              var say = List(
                c"{o}k{.|, chief}",
                c"it['ll| shall| will] [be|get] done{.}", 
                c"ay{-ay} {cap'n|captain}!", 
                c"{sure,|ok,|ok yeah,|ay,} I['ll| will]"+" "+Seq(
                  c"[tell|relay it to] ${themForm(toMsgNicks)}{.}",
                  c"make [sure|certain]"+" "+(
                    if(isHere.nonEmpty)
                      c"${theyForm(toMsgNicks)} [get|recieve]${sForm(toMsgNicks)} this {msg|message}"
                    else
                      c"this {msg|message} reaches ${themForm(toMsgNicks)} {when ${theyForm(toMsgNicks)} return${sForm(toMsgNicks)}} {here}")).random + c"{.}")
              
              if(dontMsgNicks.nonEmpty) {
                say = say.map(_ + " (" + 
                  c"{well, } [except|but not] for"+" "+Seq("the " +
                    (if(dontMsgNicks.forall(_.isFemale))
                      (if(dontMsgNicks.size == 1) c"[girl|woman|lady]" else c"[girls|women|ladies]")
                    else if(dontMsgNicks.forall(_.isMale))
                      (if(dontMsgNicks.size == 1) c"[guy|man|gentleman]" else c"[guise|guys|men|gentlemen]")
                    else if(dontMsgNicks.exists(_.isBot))
                      (if(dontMsgNicks.size == 1) c"[one]" else c"[ones]")
                    else
                      (if(dontMsgNicks.size == 1) c"[person|human]" else c"[ppl|people|humans]")
                    )+" "+c"I've just [complained about|mentioned]",
                    dontMsgNicks.init.mkString(", ")+(if(dontMsgNicks.size > 1) " and " else "")+dontMsgNicks.last
                  ).random + ")")
              }
              
              speak(say: _*)
            }
          }
        case _ =>
          speak(c"Sorry, but I {really|kind of|unfortunately} don't know what to do with this.")
      }
    } else if(message.startsWithAny("@reword ", "@rephrase ")) {
      val toReword = message.dropWhile(_ != ' ').tail
      var rephrased = wordnet.rephrase(toReword)
      def isRepost: Boolean = (rephrased == toReword || rephrased == lastMsg_)

      var maxIters = 5
      while(maxIters > 0 && isRepost) {
        rephrased = wordnet.rephrase(toReword)
        maxIters -= 1
      }

      speak(if(isRepost) "Sorry, I've got nothing..." else rephrased)
    } else if(message.startsWithAny("@context", "@tldr", "@tl;dr", "@keyword")) thread {
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
            c"I have no idea{.}1-3",
            c"I don't know what this is about{.}1-3")
        } :_*)
    } else if(message.startsWithAny("@suggest")) thread {
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
    } else if(message.contains("@all") && !users.exists(_ matches "_*botko_*")) {
      speak((users.toBuffer -- mustNotBeNamed).mkString(", "))
    } else if((message matches "@?"+name+"[:, ]{0,3}(uptime|updog)")
      || (message.startsWithAny("uptime ", "@uptime") && mentions.nonEmpty)
      || (message.startsWithAny(users.toSeq: _*) && message.split(" ").size <= 5 && message.contains("uptime"))) {
      
      val mytime = withAlternative(getSinceString(startTime), "who knows how long..")
      
      import sys.process._
      for(nick <- mentions) {
        if(nick == this.name) {
          val servertime = withAlternative(
            getSinceZeroString(Seq("cat", "/proc/uptime").!!.trim.takeWhile(_ != '.').toInt),
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
    
    // Checks twitter only every few minutes, and only if people are talking on channel
    checkTwitter()

    // Last msgs
    lastMsgs :+= message
    lastMsgs = lastMsgs.take(10)
  }
  
  val lastPrivateMessage = mutable.AnyRefMap[String, String]() withDefaultValue ""
  def speakPriv(message: String, nick: String, msgs: String*): Unit = {
    val nickClean = nick.replaceAll("[^a-zA-Z]", "") //TODO: use cleanNick + clean for filename - make File wrapper that takes care of that, get filename regex, etc., close file
    appendToFile("logs/chat_"+nickClean+".log", allowFail = true)(nickClean+" "+message)
  
    for(newMsg <- (msgs.toBuffer - lastPrivateMessage(nick)).randomOption) {
      Thread.sleep((500+nextInt(500*2)).toLong)
      sendMessage(nick, newMsg)
      lastPrivateMessage(nick) = newMsg
      appendToFile("logs/chat_"+nickClean+".log", allowFail = true)(name+" "+newMsg)
    }
  }

  override def onPrivateMessage(sender: String, login: String, hostname: String, message: String): Unit = {
    if(getUsers contains sender) message.makeEasy.replace(
      "i'm" -> "i am", 
      "i've" -> "i have", 
      "i'll" -> "i will", 
      "i'd" -> "i would",
      "i was" -> "i am",
      "gonna" -> "going to", 
      "they're" -> "they are", 
      "we're" -> "we are",
      "don't" -> "dont"
    ).split(" ").toList match {
      case List("hello") | List("hi") => speakPriv(message, sender,
          "How do you do?",
          "Hi. How are you?")
      case "i" :: "am" :: x => 
        if(x.nonEmpty && x.head.endsWith("ing")) { // doING something
          val x2 = x.map(word => if(List("my", "mine") contains word) "your" else word)
          //Memory <= (IsVerbing, x2.mkString(" "))
          speakPriv(message, sender,
            "How does "+x2.mkString(" ")+" make you feel?",
            "How long have you been "+x2.mkString(" ")+"?")
        } else if(x.nonEmpty && List("a", "an", "the").contains(x.head)) { // being A something
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
          if(x.nonEmpty && x.head == "my")
            speakPriv(message, sender,
              "Why do you think your "+x.tail.mkString(" ")+"?")
          else
            speakPriv(message, sender,
              "What makes you think that?",
              "Why do you think that is?")
      case "i" :: "feel" :: x => 
          speakPriv(message, sender,
            "How long have you been feeling"+x.mkString(" ")+"?",
            if(x.size > 1) "Does anyone else you know "+x.head+" "+x.tail.mkString(" ")+"?" else
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
            "Do you really think that about me?")
        else {
          //val memNoun = Memory->(IsNoun)
          //val memVerb = Memory->(IsVerbing)
          speakPriv(message, sender,
            //if(memNoun.isDefined) {
            //  speak(
            //    "Let's talk more about you being "+memNoun.get+".")
            //} else
            c"{Perhaps|Maybe} {you're right}",
            c"Let's change the topic{.}1-3",
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

