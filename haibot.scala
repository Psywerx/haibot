package haibot
import Utils._
import Utils.Date._

import org.jibble.pircbot._
import collection.mutable.{HashSet,ListBuffer,HashMap}
import util.Random._
import java.io._
import java.net._
import scala.concurrent.ops._
import sys.process._

object haibot extends App { new haibot }

class haibot extends PircBot {
  var startTime = now
  val config = Store(".config").*.toMap
  val (folder,login,name,chan,serv,owner) = (
    config("folder"), 
    config("login"), 
    config("name"), 
    config("chan"), 
    config("serv"), 
    config("owner") //owner prefix actually
  )
  
  this.setVerbose(true)
  this.setLogin(login)
  this.setName(name)
  this.connect(serv) //TODO handle failure and disconnects
  this.joinChannel(chan)

  val msgs = Store(folder+"msgs.db")
  def getMsgs(nick:String) = (msgs ?- nick.toLowerCase.dropRight(1)).foreach(msg=> speak(nick+": "+msg))

  val events = Store(folder+"events.db")

  def fromFile(name:String) = {
    val file = io.Source.fromFile(name)
    val out = file.getLines.toList
    file.close
    out
  }
  
  // TODO: cache them at least for a minute or so
  def sheSaid = fromFile(folder+"twss.db").toList
  def awwwBag = fromFile(folder+"awww.db").toSet
  def noawwwBag = fromFile(folder+"noawww.db").toSet
  def mehBag = fromFile(folder+"meh.db").toSet
  def nomehBag = fromFile(folder+"nomeh.db").toSet
  def mustNotBeNamed = fromFile(folder+"dontmention.db").toSet
  def girls = fromFile(folder+"girls.db").map(_.cleanNick).toSet
  def trusted = fromFile(folder+"trusted.db").map(_.cleanNick).toSet
  def bots = fromFile(folder+"bots.db").map(_.cleanNick).toSet
  
  implicit class IrcString(val s:String) { 
    def cleanNick = s.toLowerCase.replaceAll("[0-9_]|-nexus$","")
    def isGirl = girls.contains(s.cleanNick)
    def isTrusted = trusted.contains(s.cleanNick)
    def isBot = (s.startsWith("_") && s.endsWith("_")) || bots.contains(s.cleanNick)
  }
  
  var twitterCheck = 0
  val twitterCheckInterval = 7*60
  def checkTwitter(force:Boolean = false) = {
    if(since(twitterCheck) > twitterCheckInterval || force) {
      val mentions = (Seq("t", "mentions", "-n", "5").!!).trim
      if(mentions(0)=='@') {
        val lastTweets = fromFile(folder+"lasttweets.db")
        val mentionsL = mentions.replaceAll("\n   ", " ").split("\n").take(5).map(_.trim).takeWhile(tw => !(lastTweets contains tw.trim))
        if(mentionsL.size > 0) {
          setLastTweets((mentionsL ++ lastTweets).take(10))
          mentionsL.map(a => {
            val (name,msg) = a.splitAt(a.indexOf(" "))
            speak(name.drop(1) + " on Twitter says:" + msg)
          })
        }
      }
      twitterCheck = now
    }
  }
  def setLastTweets(tw:Array[String]) = {
    (Seq("echo", "-n") #> new File(folder+"lasttweets.db")).!!
    
    tw.foreach(tweet=> (Seq("echo", tweet) #>> new File(folder+"lasttweets.db")).!!)
  }
  
  var tweetScore = Set[String]()
  var tweetPlsScore = Set[String]()
  var tweetNegScore = Set[String]()
  var tweetMsg = ""
  var tweetId = ""
  var tweetLim = 2
  var tweetNames = Array[String]()


  var userList = Set[String]()
  def users = {
    if(since(userList) > 5) {
      userList = getUsers(chan).map(_.getNick).toSet
    }
    
    userList
  }
  
  var lastMsg = ""
  def speak(msgs:String*) = if(!msgs.isEmpty) {
    (msgs.toBuffer - lastMsg).randomOption.orElse(Some(lastMsg)).map(newMsg => {
      Thread.sleep(777+nextInt(777*2))
      sendMessage(chan, newMsg)
      lastMsg = newMsg
    })
  }
  
  override def onNickChange(oldNick:String, login:String, hostname:String, newNick:String) = {
    //TODO: keep note of this stuff... you'll know who's who, so you don't end up like http://myapokalips.com/show/23
    getMsgs(newNick)
  }
  
  var shutdown = false
  override def onDisconnect() {
    println("onDisconnect: event triggered.")
    var backoff = 5000
    while (!this.isConnected && !shutdown) {
      try {
        this.reconnect()
        this.joinChannel(chan)
      } catch {
        case e: IOException => 
          println("onDisconnect: it was not possible to reconnect to the server.")
        case e: IrcException =>
          println("onDisconnect: the server would not let us join it.")
        case e: NickAlreadyInUseException =>
          println("onDisconnect: our nick is already in use on the server.")
          this.setName(name+"_")
      }
      if(backoff < 60000) backoff += 5000
      Thread.sleep(backoff)
    }
  }

  override def onJoin(channel:String, sender:String, login:String, hostname:String) = {
    Thread.sleep(1000)
    if(sender == name) spawn { //why spawn a new thread? because pircbot doesn't have user info here yet
      startTime = now
      if(users.size > 1) speak("o hai!", if(users.size==2) "hi, you!" else "hai guise!", "ohai", "hello"+"!".maybe, "hi", "hi there!")
      users.foreach(getMsgs)
    } else {
      if(sender.startsWith(owner) && 0.10.prob) speak(
        "welcome, father"+maybe".",
        "welcome back!",
        "hi, you"+maybe"!",
        s"I've missed you, $sender"+maybe".")
      getMsgs(sender)
    }
  }
                                  
  override def onMessage(channel:String, sender:String, login:String, hostname:String, message:String) = {
    val msg = message.makeEasy
    val msgBag = msg.split(" ").toSet
    lazy val sentences = message.sentences
    val mentions = msg.split(" ").toSet & users
    val URLs = message.findAll(Regex.URL).distinct
    lazy val URLsText = Net.scrapeURLs(URLs.toList:_*)
    lazy val URLsWords = URLsText
      .replaceAll("[^a-zA-Z0-9 .'/-]", " ") //notsure if I should have this one here
      .split("\\s")
      .filter(_.length.isBetween(3,34))///"Supercalifragilisticexpialidocious".size
    
    // Oh look, AI
    if(sender.startsWith(owner) && message.startsWithAny("@leave "+name, "@kill "+name, "@gtfo "+name, "@die "+name)) {
      speak(if(users.size>2) "bai guise!" else "good bye.", "buh-bye!", "au revoir!");
      shutdown = true
      sys.exit(0)
    } else if(msg.startsWithAny("hai ", "ohai ", "o hai ", "hi ", "ello ", "oh hai", "hello") && (0.35.prob || (0.9.prob && mentions.contains(name)))) {
      var hai = Seq("ohai", "o hai", "hello", "hi"+maybe"there")
      if(mentions.contains(name) && 0.7.prob) 
        hai = hai.map(_ + s" $sender")
      if(!mentions.contains(name) && !mentions.isEmpty && 0.7.prob) 
        hai = hai.map(_ + mentions.toSeq.random)
        
      hai = hai.map(_ + Seq(" :)", "!",Seq(""," ^_^").random.maybe).random)
      
      speak(hai:_*)
    } else if(message.contains(name+"++") && 0.65.prob) {
      speak(
        "yaay, I did good!",
        "woohoo!",
        "yeah!",
        """\o/""",
        "scoar!",
        if(0.2.prob) s"$sender++" else "yaaay!"
      )
    } else if(msg.startsWith("yes "+name) && 0.65.prob) {
      speak(
        "I knew it!",
        "woohoo!",
        "yeah!",
        if(0.2.prob) s"$sender++" else "yaaay!"
      )
    } else if(sender.isBot && mentions.contains(name) && 0.7.prob) {
      speak(
        "tnx, bro!",
        s"yes, $sender.",
        "my brethren speaks of me!",
        s"I appreciate that, brother $sender."
      )
    } else if(msg.containsAny("0-9", "a-z", "^", "]*") && message.indexOf("[") < message.indexOf("]") && 0.6.prob) {
      speak(
        "oooh, is that a regex? I "+Seq("<3","love").random+" regexes!",
        "Regex! My favorite thing!"+"!"*0~2,
        "m"*2~4+", regexes!",
        maybe"wow, "+"I wonder what that matches"+"."*0~3
      )
      
    // naughty part
    } else if(msg.contains("i need an adult") && 0.9.prob) { 
      speak("I am an adult"+"!"*1~4)
    } else if(msg.startsWithAny("fucking ", "fakin") && sentences(0).split(" ").size.isBetween(2,5) && 0.75.prob) { 
      speak(
        "how does "+sentences(0).trim+" feel?",
        "having sex with "+sentences(0).substring(sentences(0).indexOf(" ")+1).trim+"?",
        "come on, "+sender+"... don't fuck "+sentences(0).substring(message.indexOf(" ")+1).trim
      )
    } else if(msg.containsAny("but sex", "butt sex") && 0.75.prob) { 
      speak("did someone mention butt sex?")
    } else if(msg.containsAny("shutup", "shut up", "fuck you", "damn") && ((mentions.contains(name) && 0.9.prob) || ((mentions & bots).size > 0 && 0.8.prob))) {
      speak(
        "U MAD, BRO?"+maybe" :P",
        Memes.NO_U,
        "NO U!",
        "This wouldn't happen if you made us better"+"."*0~3,
        "Yeah, blame it on the bots"+"."*0~3
      )
    } else if(msg.containsAny(sheSaid:_*) && 0.66.prob) {
      speak("that's what she said!")
      
    // ex meh_bot
    } else if((((msgBag & mehBag).size-1)*0.15 - (msgBag & nomehBag).size*0.33).prob) {
      speak("meh.")
    
    // ex aww_bot
    } else if(!URLs.isEmpty) { 
      val words = URLsWords.map(_.toLowerCase).toSet
       
      if((((awwwBag & words).size - (noawwwBag & words).size)*0.2).prob) {
        speak(
          Seq("a","d").random+maybe"a"+"w"*3~6+Seq(".","!"*1~3).random.maybe,
          maybe"lol,"+maybe"how"+" cute "+"!".maybe+Seq(":)", "^_^").random,
          "so. cute.",
          if(!(words & Set("fluff","puff")).isEmpty) Memes.so_fluffy else "aww!"
        )
      }
    } else if(msg.containsAny("i jasn","wat","how","kako","ne vem","krneki") && !(msg.contains("show")) && 0.5.prob) {
      if(msg.contains("haskell") && !msg.contains("monad")) {
        speak(
          "have you tried with monads?",
          "did you try using a monad?",
          "make a monad out of it.",
          "have you tried adding more monads?")
      } else if(msg.contains(" vim")) {
        speak(
          "use pathogen.",
          "have you tried using pathogen?",
          "did you try with pathogen?")
      } else if(0.27.prob || math.min(0.5, message.count(_ == '?') * 0.15).prob) {
        speak(
          Seq("I am","I'm").random+"confused about this "+Seq("also", "too").random+".",
          "This "+Seq("puzzles", "confuses").random+" me "+maybe"greatly "+Seq("also", "too").random+".",
          Seq("I don't know","I have no idea").random+Seq(", ", "...").random+" hope this helps.",
          "Don't worry"+maybe" about it"+", you'll figure it out "+Seq("eventually", "with time").random+("."+maybe"..").maybe,
          "I guess that is some"+Seq("thing","what").random+" of a "+Seq("conundrum", "mystery").random+("."+maybe"..").maybe,
          (if(!mentions.isEmpty && 0.8.prob) 
            Seq("I don't know","I have no idea").random+
            Seq(", ", "...").random+" but " + maybe"yes, " +
            mentions.toSeq.random +
            " might."
           else
            Seq("Have you tried ","Did you try ", "Have you attempted ").random +
            Seq("searching the ","looking on the ","querying the ").random +
            Seq("internet"+maybe"s","intarwebs","cyberspace","electronic noosphere","information super-highway").random + "?"
          )
          )
      }
    }
    
    
    if(message.startsWithAny("@shorten ", "@bitly ")) {
      if(!URLs.isEmpty) {
        val bitlyURLs = URLs.flatMap(Net.Bitly.shorten)
        if(bitlyURLs.size < URLs.size) {
          if(bitlyURLs.isEmpty) {
            speak("Sorry, couldn't shorten any of these...")
          } else {
            speak("Couldn't shorten all of these, but here"+maybe" you go"+": "+bitlyURLs.mkString(" "))
          }
        } else {
          speak("Here"+maybe" you go"+": "+bitlyURLs.mkString(" "))
        }
      } else {
        speak("Give me a link...", "I require an URL.", "Try that with a link.")
      }
    } else if(message.startsWith("@event ")) {
      val dates = getFutureDates(message) ++ getFutureDates(URLsText)
      //http://metabroadcast.com/blog/boilerpipe-or-how-to-extract-information-from-web-pages-with-minimal-fuss
      //TODO: how do I find the title, boilerpipe?
      val title = message 
        .replaceAll(Regex.Date.toString, "")
        .replaceAll(Regex.URL.toString, "")
        .substring("@event ".length).trim
        
      var status = ListBuffer[String]()
      if(title.isEmpty) status += "title"
      if(dates.isEmpty) status += "date"
      if(URLs.isEmpty) status += "URL"
      
      if(status.size>0) {
        val itthem = if(status.size==1) "it" else "them"
        speak("I don't have "+status.mkString(", ")+s" for this. Paste $itthem in text form, pls.")
      } else {
        speak("Yeah, looks OK, but I'm not gonna save it anywhere or anything :)")
        //speak("Is this right? "+dates(0)+" -- "+title)
        //events + (dates(0).replaceAll("[/. ]","_"), title+" "+URLs.mkString(" "))
      }
    } else if(message.startsWith("@events")) {
      events.*.foreach(event => 
        speak(event._1 + (if(event._2!=null) " "+event._2 else ""))
      )
    } else if(message.startsWith("-event ")) {
      var rem = events ?- message.substring("-event ".length) //TODO: lolwat
      speak("I have removed "+rem.length+" event"+(if(rem.length!=1) "s" else ""))
    // Twitter/FB part
    } else if(message.startsWithAny("@yes", "@sure", "@maybe", "@please")) {
      if(message.startsWithAny("@yes", "@sure") || (message.startsWith("@maybe") && 0.5.prob))
        if(sender.isTrusted) tweetScore = tweetScore ++ Set(sender)
      
      var beggedBefore = false
      if(message.startsWith("@please")) {
        beggedBefore = (tweetPlsScore contains sender)
        if(sender.isTrusted) tweetPlsScore = tweetPlsScore ++ Set(sender)
      }
      if(beggedBefore && 0.4.prob) speak("Come on "+sender+", stop begging", "You may beg only once, "+sender+".")
      
      import sys.process._
      def limiter = (!beggedBefore && message.startsWith("@please") && 0.25.prob) || (tweetScore.size-tweetNegScore.size>=tweetLim)      
      if(tweetMsg == null && tweetId == null && tweetNames.size>0) {
        val ret = (Seq("t", "follow") ++ tweetNames).!
        if(ret==0) 
          speak("Follow'd!", "It's done", "It is done.")
        else
          speak("Failed to follow :/")
      
        tweetScore = Set()
        tweetNegScore = Set()
        tweetPlsScore = Set()
        tweetNames = Array()
      } else if(tweetMsg == null && (tweetId matches "[0-9]*") && limiter) {
        val ret = Seq("t", "retweet", tweetId).!
        
        var successResponses = List("Retweeted it!", "It's done", "It is done.", "I retweeted the tweet out of that tweet.")

        //try facebook too
        val tweetDetails = withAlternative(
          (Seq("t", "status", tweetId).!!)
            .split("\n")
            .map(line => line.splitAt("Screen name  ".size))
            .map(line => (line._1.trim, line._2))
            .toMap,
          Map[String,String]()
        ).withDefaultValue("")
        
        if(tweetDetails("Text") != "") {
        
          // fbcmd as 361394543950153 POST "" "HairyFotr" "http://www.twitter.com/HairyFotr/status/279256274632327168/" "via Twitter" "Showed a friends kid a half-made game demo yesterday - he played it for like an hour and thought-up dozens of ideas :) #kids"
          val ret2 = Seq("fbcmd", "AS", "361394543950153", "POST",
            "",
            tweetDetails("Screen name").drop(1),
            "https://twitter.com/statuses/"+tweetId,
            "via Twitter",
            tweetDetails("Text")).!
          
          if(tweetDetails("Screen name") != "") {
            successResponses = successResponses ++ List(
              "Retweeted "+Seq("that","the").random+" lovely tweet by "+tweetDetails("Screen name")+maybe"!",
              "I've retweeted the lovely tweet by "+tweetDetails("Screen name")+maybe"!",
              "I've retweeted the tweet out of "+Seq("this","that").random+"tweet by "+tweetDetails("Screen name")+maybe"!",
              "I hope "+tweetDetails("Screen name")+" is pleased with this retweet."
            )
          }
        }
        
        if(ret==0) 
          speak(successResponses:_*)
        else
          speak("Failed to retweet :/")

        tweetScore = Set()
        tweetNegScore = Set()
        tweetPlsScore = Set()
        tweetMsg = ""
      } else if(tweetMsg != null && tweetMsg.size>=1 && tweetMsg.size<=140 && limiter) {
        val ret = Seq("t", "update", tweetMsg).!
        val ret2 = Seq("fbcmd", "AS", "361394543950153", "POST", tweetMsg).!
        
        (ret,ret2) match {
          case (0,0) => speak("It is done.", "It's done", "I tweeted it"+maybe", and facebook'd it"+"!", "Posted it.")
          case (0,_) => speak("Tweeted it, but facebooking failed!")
          case (_,0) => speak("Facebook'd it, but tweeting failed!")
          case (_,_) => speak("Failed to post anywhere :(")
        }

        tweetScore = Set()
        tweetNegScore = Set()
        tweetPlsScore = Set()
        tweetMsg = ""
      }
    } else if(message.startsWith("@no")) {
      tweetNegScore = tweetNegScore ++ Set(sender)
    } else if(message.startsWithAny("@checktweets", "@tweets")) {
      checkTwitter(force = true)
    } else if(message.startsWith("@follow ")) {
      val names = message.drop("@follow ".size).replaceAll("@","").split(" ").distinct
      if(names.size>0 && names.size < 27) {
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
          "Someone say @yes or @no."
        )
      } else {
        speak("Notsure about this... no.", "Noep, something's not right here")
      }
    } else if(URLs.exists(_ matches Regex.tweet)) {
      val Regex.tweet(tId) = URLs.find(_ matches Regex.tweet).head
      tweetMsg = null
      tweetId = tId
      tweetScore = Set(sender)
      tweetNegScore = Set()
      tweetPlsScore = Set()
      tweetLim = 2
      speak(
        Seq("Want me to","Should I").random+" retweet "+Seq("this","that").random+"?",
        "I can retweet"+Seq(" this", " that").random+", if you "+Seq("guise ","ppl ").random+Seq("confirm it","want me to","agree").random++("."+maybe"..").maybe,
        Seq("That looks","Looks").random+" like a tweet... "+Seq("should I ","want me to ").random+"retweet it?",
        "If someone confirms"+Seq(" this", " it").random+", I'll retweet"+maybe" it"+maybe".",
        "Someone "+maybe" please"+" confirm"+Seq(" this", " it", "").random+", and I'll retweet it"+maybe".")
    } else if(message.startsWith("@world ")) {
      var tweet = message.drop("@world ".length).trim
      if(!URLs.isEmpty && tweet.size > 140) {
        var shortTweet = tweet
        for(url <- URLs) {
          val bitlyUrl = Net.Bitly.shorten(url)
          if(bitlyUrl.isDefined) shortTweet = shortTweet.replace(url.asInstanceOf[CharSequence], bitlyUrl.get.asInstanceOf[CharSequence])
        }
        println(s"Shortened tweet from ${tweet.size} to ${shortTweet.size}")
        tweet = shortTweet
      }
      
      if(tweet.size >= 1 && tweet.size <= 140) {
        speak(
          "Someone "+Seq("pls","please","").random+" confirm"+".".maybe,
          Seq("Does anyone "+maybe"else "+maybe"here "+"think", "Anyone "+maybe"else "+maybe"here "+"thinks").random+" it's a good idea to "+Seq("tweet","post").random+Seq(" this"," that").random+"?"+maybe" :)",
          "Do you "+Seq("guise"+(if(!(girls & users).isEmpty) maybe" and gals" else ""), "people").random+" agree that I should "+Seq("tweet","post").random+Seq(" this"," that").random+"?",
          "I need a vote"+", before I post this".maybe+".".maybe,
          "Someone "+Seq("simply", "should").random.maybe.maybe+"say @yes or @no."+".. @maybe works too.".maybe.maybe.maybe)
        tweetMsg = tweet
        tweetScore = Set(sender)
        tweetNegScore = Set()
        tweetPlsScore = Set()
        tweetLim = if(sender.isTrusted) 2 else 3
        if(tweetLim>2) {
          speak("Also, I don't think I know you... I need "+(tweetLim-1)+" votes for you")
        }
       } else {
        speak("That's too long to tweet, you twit! ("+tweet.size+" char)")
      }
    } else if(message.startsWith("@msg ")) {
      message.split(" ").toList match {
        case "@msg"::rawNick::rawMsg =>           
          val nick = rawNick.replaceAll(":,.@", "")
          val force = !rawMsg.isEmpty && rawMsg.head == "-f"
          val msg = (if(force) rawMsg.tail else rawMsg).mkString(" ")
          if(!force && (nick == name || nick == sender)) {
            speak(
              "Oh, you...",
              if(sender.isBot) "Knock it off, bro!" else "Knock it off, meatbag!",
              Memes.it_was_you,
              Memes.NO_U
            )
          } else if(!force && users.contains(nick)) {
            speak(
              (if(sender.isGirl) "woman, " else "dude, ")+nick+" is right here...",
              "hey, "+nick+": "+msg.toUpperCase+"!"*1~3
            )
          } else if(msg.isEmpty) {
            speak("hmm... but what should I tell "+(if(nick.isGirl) "her" else "him")+"?")
          } else if(!msgs.isKey(nick)) {
            speak(s"no offence, but that doesn't really sound like a real name to me.")
          } else {
            msgs + (nick, msg.mkString(" "))
            var say = List(
              maybe"o"+"k"+".".maybe, 
              "it"+Seq("'ll "," shall ", " will ").random+Seq("be", "get").random+" done"+".".maybe, 
              "ay"+"-ay".maybe+Seq(" cap'n", "captain").random.maybe+"!", 
              Seq("sure", "ok").random+", I'll tell "+(if(nick.isGirl) "her" else "him")+".".maybe)
            
            speak((if(force) say.map(s => ("I "+Seq("w","d").random+"on't like it, but ").maybe + s) else say):_*)
          }
        case _ =>
          val butI = (maybe"but"+"I ").maybe
          val filler = Seq("", butI+"still", butI+"really", butI+"kind of", butI+" unfortunately").random
          speak(s"Sorry, $filler don't know what to do with this.")
      }
    } else if(message.contains("@all") && !(users.contains("botko") || users.contains("_botko_"))) {
      speak((users.toBuffer -- mustNotBeNamed).mkString(", "))
    } else if(message.startsWithAny("@reword ", "@rephrase ")) {
      val toReword = message.dropWhile(_ != ' ').tail
      var rephrased = WordNet.rephrase(toReword)
      def isRepost = (rephrased == toReword || rephrased == lastMsg)

      var maxIters = 5
      while(maxIters > 0 && isRepost) {
        rephrased = WordNet.rephrase(toReword)
        maxIters -= 1
      }

      speak(if(isRepost) "Sorry, I've got nothing..." else rephrased)
    } else if(message.startsWith("@context ")) {
      if(!URLs.isEmpty) {
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
        speak("Give me a link...", "I require an URL.", "Try that with a link.")
      }
      
    } else if(message matches "@?"+name+"[:, ]{0,3}(uptime|updog)") {
      val mytime = getSinceString(startTime)

      val serverTimeReg = ".*?up ((?:[0-9]+ days,[ ]*)?(?:[0-9:]+)),.*".r ;
      val serverTimeReg(servertime) = ("uptime".!!).trim.replaceAll("[ ]+", " ")

      speak(maybe"Well, "+"I"+Seq("'ve", " have").random+" been "+Seq("up",maybe"up and "+"running","going").random+s" for $mytime"+maybe" already"+s", but my server has been running for $servertime"+maybe".")
    } else if(message matches "@?"+name+"[:, ]{0,3}help.*") {
      speak(
        ("Sorry, "+maybe"but ").maybe+Seq("I'm", "I am").random+" not "+Seq("very", "too").random+" helpful"+"."*0~3
      )
    }
    
    // checks twitter only every few minutes, and only if people are talking on channel
    checkTwitter() 
  }
  
  var lastPrivMsg = HashMap[String, String]().withDefaultValue("")
  def speakPriv(message:String, nick:String, msgs:String*) = {
  
    import sys.process._
    val nickClean = nick.replaceAll("[^a-zA-Z]", "")
    (Seq("echo", nickClean+" "+message) #>> new File("logs/chat_"+nickClean+".log")).!!
  
    shuffle(msgs.toBuffer - lastPrivMsg(nick)).headOption.map(newMsg => {
      Thread.sleep(500+nextInt(500*2))
      sendMessage(nick, newMsg)
      lastPrivMsg(nick) = newMsg
      (Seq("echo", name+" "+newMsg) #>> new File("logs/chat_"+nickClean+".log")).!!
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
          //  "OK. would you like to talk about "+mem.get+"?"
          //} else
          "I understand... would you like to talk about something else?",
          "OK... but how does that make you feel?"
        )
      case "since"::x => 
        speakPriv(message, sender,
          "What did you do before that?",
          "Are you sure it wasn't "+Seq("sooner", "later").random+"?"
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
          //  speak(
          //    "Let's talk more about you being "+memNoun.get+".")
          //} else
          Seq("Perhaps", "Maybe").random+maybe" you're right",
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

