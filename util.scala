package org.psywerx

import collection.mutable.{HashSet,ListBuffer,HashMap}
import java.io._
import java.net._
import math._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.util._
import scala.concurrent.duration._
import scala.util.matching
import scala.util.matching._
import scala.util.Random._

object util {
  val folder = "util/"
  
  def withAlternative[T](func: => T, alternative: => T ): T = try { func } catch { case _: Throwable => alternative}
  def withExit[T](func: => T, exit: => Any = { }): T = try { func } catch { case _: Throwable => exit; sys.exit(-1) }
  def tryOption[T](func: => T): Option[T] = try { Some(func) } catch { case _: Throwable => None }
  
  implicit class PimpString(val s: String) { 
    def replaceAll(m: (String, String)*): String = m.foldLeft(s)((out,rep)=> out.replaceAll(rep._1,rep._2))
    // TODO: containsPercent: Double for fuzzy reasoning
    def containsAny(strs: String*): Boolean   = strs.foldLeft(false)((acc,str) => acc || s.contains(str))
    def startsWithAny(strs: String*): Boolean = strs.foldLeft(false)((acc,str) => acc || s.startsWith(str))
    def endsWithAny(strs: String*): Boolean   = strs.foldLeft(false)((acc,str) => acc || s.endsWith(str))
    def sentences: Array[String] = s.split("[.!?]+") // TODO: http://stackoverflow.com/questions/2687012/split-string-into-sentences-based-on-periods
    def makeEasy: String = s.toLowerCase.map(char => ("čćžšđ".zip("cczsd").toMap).getOrElse(char, char)).replaceAll("[,:] ", " ") // lazy way to make text processing easier
    def maybe: String = if(nextBoolean) s else ""
    def findAll(r: Regex): List[String] = r.findAllIn(s).toList
    //def removeAll(rem: String): String = s.filterNot(rem contains _) //notsure wat
    def matches(r: Regex): Boolean = s.matches(r.toString)
    def distance(s2: String): Int = distance(s,s2)
    def distance(s1: String, s2: String): Int = {
      val memo = scala.collection.mutable.Map[(List[Char],List[Char]),Int]()
      def min(a: Int, b: Int, c: Int) = Math.min( Math.min( a, b ), c)
      def sd(s1: List[Char], s2: List[Char]): Int = {
        if(memo.contains((s1,s2)) == false)
          memo((s1,s2)) = (s1, s2) match {
            case (_, Nil) => s1.length
            case (Nil, _) => s2.length
            case (c1 :: t1, c2 :: t2) => 
              min(
                sd(t1,s2) + 1,
                sd(s1,t2) + 1,
                sd(t1,t2) + (if(c1 == c2) 0 else 1))
          }
        memo((s1,s2))
      }

      sd( s1.toList, s2.toList )
    }
  }
  
  implicit class MaybeSI(val sc: StringContext) extends AnyVal { def maybe(args: Any*): String = sc.parts.iterator.mkString("").maybe }
  implicit class PimpInt(val i: Int) extends AnyVal { def ~(j: Int) = nextInt(j-i+1)+i }

  implicit class Seqs[A](val s: Seq[A]) { 
    def random: A = s(nextInt(s.size)) 
    def randomOption: Option[A] = if(s.size > 0) Some(random) else None
  }

  implicit class D(val d: Double) { def prob: Boolean = nextDouble < d } //0.5.prob #syntaxabuse
  implicit class F(val f: Float) { def prob: Boolean = nextFloat < f }
  implicit class I(val i: Int) { def isBetween(min: Int, max: Int): Boolean = i >= min && i <= max}

  object Regex {
    val URL = """(?i)\b((?:[a-z][\w-]+:(?:/{1,3}|[a-z0-9%])|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'".,<>?«»“”‘’]))""".r
    val tweet = "https?://(?:www\\.)?twitter\\.com/.*/status(?:es)?/([0-9]+).*".r
  }

  object Memes {
    val NO_U = """http://bit.ly/yaJI5L"""
    val oh_you = """http://bit.ly/MvTwUG"""
    val so_fluffy = """http://bit.ly/MG5Hfx"""
    val it_was_you = """http://bit.ly/zg0jQt"""
  }
    
  object Date {
    //wish I knew a better way.
    import java.util.Date
    import java.text._
    
    private val timeReg = """[\s,]{0,2}(ob\s)?((\d{1,2}:\d{2})|(\d{1,2}h))"""
    private val dateFormats = List[(matching.Regex, SimpleDateFormat)](
      (("""\d{1,2}[.]\d{1,2}[.]\d{4}""" + timeReg).r, new SimpleDateFormat("dd.MM.yyyy HH:mm")),
      (("""\d{1,2}-\d{1,2}-\d{4}""" + timeReg).r, new SimpleDateFormat("dd-MM-yyyy HH:mm")),
      (("""\d{4}-\d{1,2}-\d{1,2}""" + timeReg).r, new SimpleDateFormat("yyyy-MM-dd HH:mm")),
      (("""\d{1,2}/\d{1,2}/\d{4}""" + timeReg).r, new SimpleDateFormat("MM/dd/yyyy HH:mm")),
      (("""\d{4}/\d{1,2}/\d{1,2}""" + timeReg).r, new SimpleDateFormat("yyyy/MM/dd HH:mm")),
      (("""\d{1,2}\s[a-z]{3}\s\d{4}""" + timeReg).r, new SimpleDateFormat("dd MMM yyyy HH:mm")),
      (("""\d{1,2}\s[a-z]{4,}\s\d{4}""" + timeReg).r, new SimpleDateFormat("dd MMMM yyyy HH:mm")),
      (("""\d{1,2}[.]\s[a-z]{4,}\s\d{4}""" + timeReg).r, new SimpleDateFormat("dd. MMMM yyyy HH:mm")),
      ("""\d{1,2}[.]\d{1,2}[.]\d{4}""".r, new SimpleDateFormat("dd.MM.yyyy")),
      ("""\d{1,2}-\d{1,2}-\d{4}""".r, new SimpleDateFormat("dd-MM-yyyy")),
      ("""\d{4}-\d{1,2}-\d{1,2}""".r, new SimpleDateFormat("yyyy-MM-dd")),
      ("""\d{1,2}/\d{1,2}/\d{4}""".r, new SimpleDateFormat("MM/dd/yyyy")),
      ("""\d{4}/\d{1,2}/\d{1,2}""".r, new SimpleDateFormat("yyyy/MM/dd")),
      ("""\d{1,2}\s[a-z]{3}\s\d{4}""".r, new SimpleDateFormat("dd MMM yyyy")),
      ("""\d{1,2}\s[a-z]{4,}\s\d{4}""".r, new SimpleDateFormat("dd MMMM yyyy")),
      ("""\d{1,2}[.]\s[a-z]{4,}\s\d{4}""".r, new SimpleDateFormat("dd. MMMM yyyy")))

    private def deLocale(dateStr: String): String = 
      dateStr
        .replaceAll("januar(ja|jem)?", "january")
        .replaceAll("februar(ja|jem)?", "february")
        .replaceAll("marec|marc(a|em)?", "march")
        .replaceAll("april(a|om)?", "april")
        .replaceAll("maj(a|em)?", "may")
        .replaceAll("junij(a|em)?", "june")
        .replaceAll("julij(a|em)?", "july")
        .replaceAll("avgust(a|om)?", "august")
        .replaceAll("september|septemb(ra|rom)", "september")
        .replaceAll("oktober|oktob(ra|rom)", "october")
        .replaceAll("november|novemb(ra|rom)", "november")
        .replaceAll("december|decemb(ra|rom)", "december")
        .replaceAll("h$", ":00")
        .replaceAll("[,\\s]ob[\\s]", " ")
        .replaceAll("[,\\s]+", " ")

    def getDates(text: String, filter: (Date => Boolean) = (d: Date) => true): List[Date] = {
      (dateFormats flatMap { case (regex, format) => 
        text
          .findAll(regex)
          .flatMap(dateStr => tryOption(format.parse(deLocale(dateStr))))
          .filter(filter) 
      } distinct) sortWith { (a,b) => b after a }
    }

    def getFutureDates(text: String): List[Date] = {
      val nowDate = new Date
      getDates(text, _.after(nowDate))
    }
    
    private val timeDivisor = 1000000L*1000L
    def now: Int = (System.nanoTime/timeDivisor).toInt
    def since(time: Int): Int = now-time
    def time(func: => Unit): Int = {
      val startTime = now
      func
      now-startTime
    }
    private val sinceTimes = HashMap[Int,Int]()
    def since(timeRef: AnyRef): Int = {
      val time = sinceTimes.getOrElseUpdate(timeRef.hashCode, 0)
      val nowTime = now
      sinceTimes(timeRef.hashCode) = nowTime
      
      nowTime-time
    }
    
    // used for uptime
    private def pad(i: Int, p: Int = 2): String = "0"*(p-i.toString.size)+i.toString
    def getSinceZeroString(time: Int): String = {
      val secs = time
      val (days, hours, minutes, sec) = ((secs/60/60/24), pad((secs/60/60)%24), pad((secs/60)%60), pad((secs)%60))
      
      ((days match {
        case 0 => ""
        case 1 => s"$days day "
        case _ => s"$days days "
      }) + (if(secs < 90) s"$secs seconds" else if(minutes.toInt < 60 && hours.toInt == 0 && days == 0) s"${minutes.toInt} min" else s"$hours:$minutes"))
    }
    def getSinceString(time: Int): String = getSinceZeroString(since(time))
  }
  
  object Net {
    import de.l3s.boilerpipe.extractors._
    import java.net._
    val extractor = KeepEverythingExtractor.INSTANCE
    
    //TODO: use file -i or some proper mime solution, this is risky as fuck
    def badExts: List[String] = io.Source.fromFile(folder+"badexts.db").getLines.toList
    def scrapeURLs(urls: String*): String = {
      urls.map(
        _.findAll(Regex.URL).map(url => 
          try {
            if(!url.endsWithAny(badExts: _*)) 
              extractor.getText(new URL(url))
            else 
              ""
          } catch { 
            case e: Exception => 
              e.printStackTrace()
              "" 
          }
        ).fold("")(_+" "+_)
      ).fold("")(_+" "+_).trim
    }
    
    def download(url: String, outFile: String): Boolean = {
      try {
        import java.io._
        import java.nio._
        import java.nio.channels._

        Await.ready(future {
          val Url = new URL(url)
          val rbc = Channels.newChannel(Url.openStream())
          val fos = new FileOutputStream(outFile)
          fos.getChannel.transferFrom(rbc, 0, 1 << 24)
          fos.close
        }, 30.seconds)
        
        true
      } catch {
        case e: Exception => false
      }
    }
    
    object Zemanta {
      import com.zemanta.api.{Zemanta,ZemantaResult}
      import com.zemanta.api.suggest.{Article,Keyword,Image}
      import scala.collection.JavaConversions.mapAsJavaMap
      import scala.collection.JavaConversions.asScalaBuffer
      
      def suggestKeywords(text: String, cnt: Int = 3): Option[List[String]] = suggest(text).map(_.getConfidenceSortedKeywords(true).toList.map(_.name).take(cnt))
      def suggestArticles(text: String): Option[List[Article]] = suggest(text).map(_.getConfidenceSortedArticles(true).toList)
      def suggestImages(text: String): Option[List[Image]] = suggest(text).map(_.getConfidenceSortedImages(true).toList)
      
      def suggest(text: String): Option[ZemantaResult] = {
        try {
          val apiKey = io.Source.fromFile(folder+"zemapikey").getLines.next.trim
          val zem = new Zemanta(apiKey, "http://api.zemanta.com/services/rest/0.0/");	
          val request = new java.util.HashMap[String, String](Map(
            "method" -> "zemanta.suggest",
            "api_key" -> apiKey,
            "text" -> text,
            "format" -> "xml"
          ))

          val zemResult = zem.suggest(request)
          if(!zemResult.isError) {
	          Some(zemResult)
          } else {
            None
          }
        } catch {
          case e: Exception => 
            e.printStackTrace
            None
        }
      }
    }

    object Bitly {
      import com.rosaloves.bitlyj._
      import com.rosaloves.bitlyj.Bitly._
      private val apiKey = io.Source.fromFile(folder+"bitlyapikey").getLines.next.trim.split(" ")
      
      lazy val bitly = as(apiKey(0), apiKey(1))

      def shorten(s: String): Option[String] = {
        try {
          val out = bitly.call(com.rosaloves.bitlyj.Bitly.shorten(s))
          if(out != null) Some((out.getShortUrl)) else None
        } catch {
          case e: Throwable =>
            e.printStackTrace
            None
        }
      }
      
      // try, or else return original
      def tryShorten(s: String): String = shorten(s).orElse(Some(s)).get
      
    }
  }

  /// Wordnet stuff - also in bash, and on prolog data
  object WordNet {
    //These take a while at startup... make them lazy if it bothers you
    //format is (id,word,type)... I should probably keep type
    type wn_s = (Int, String)
    val (wn_sById, wn_sByWord) = {
      val wn_sAll = io.Source.fromFile("lib/prolog/wn_s2.db").getLines.map(line => (line.take(9).toInt, line.drop(11).init.replaceAll("''","'"))).toList
      (wn_sAll.groupBy(_._1).withDefaultValue(List[wn_s]()), wn_sAll.groupBy(_._2).withDefaultValue(List[wn_s]()))
    }
    
    val wordReg = """([^a-zA-Z0-9 .'/-]*)([a-zA-Z0-9 .'/-]{3,})([^a-zA-Z0-9 .'/-]*)""".r
    val capital = """[A-Z][a-zA-Z0-9 .'/-]*"""
    val upperCase = """[A-Z0-9 .'/-]+"""
    val lowerCase = """[a-z0-9 .'/-]+"""
    
    def synonyms(strs: String*): List[String] = strs.map(synonym).toList
    def synonym(str: String): String = str match {
      case wordReg(prefix, word, suffix) => 
        val (hasCapital, allUpper, allLower) = (word matches capital, word matches upperCase, word matches lowerCase)
        val part = // percent of uppercase... fOr PEopLe wHo wrItE lIke THis
          if(allUpper) {
            1d
          } else if(allLower) {
            0d
          } else {
            val upper = word.count(char => char == Character.toUpperCase(char)).toDouble
            val lower = word.count(char => char == Character.toLowerCase(char)).toDouble
            upper/(upper+lower)
          }

        val synonyms = wn_sByWord(word.toLowerCase) // find word
          .map(_._1).distinct.flatMap(id => wn_sById(id).map(_._2)) // query all synonyms by id
          .filter(w => w != word && w.split(" ").forall(_.size >= 4)).distinct // filter probably useless ones
        
        if(synonyms.size > 0) {
          var outWord = synonyms.random
          
          if(outWord.toLowerCase == word.toLowerCase)
            outWord = word
          else if(allLower) 
            outWord = outWord.toLowerCase
          else if(allUpper) 
            outWord = outWord.toUpperCase
          else if(hasCapital && part < 0.25)
            outWord = outWord.capitalize
          else if(outWord matches lowerCase) {
            outWord = outWord.map(char => if(part.prob) Character.toUpperCase(char) else Character.toLowerCase(char)) //see part
            if(hasCapital) outWord = outWord.capitalize
          }
          
          prefix + outWord + suffix
        } else {
          str
        }
      case _ => str
    }
    def rephrase(s: String): String = synonyms(s.split(" "): _*).mkString(" ")
    
    // bad excuse for a stemmer :)
    // Note: adds the word itself in output
    def stem(word: String): List[String] = 
      (List(word) ++ 
        word
          .split("-")
          .flatMap(w => List(w, w.replaceAll("ability$", "able")))
          .flatMap(w => List(w, w.replaceAll("s$|er$|est$|ed$|ing$|d$", "")))
          //.flatMap(w => List(w, w.replaceAll("^un|^im|^in", "")))
          .filter(_.size >= 4)
      ).distinct
      
    def preprocess(word: String): List[String] = stem(word).flatMap(w => List(w, w.toLowerCase)).distinct
    
    def getMapFromwn(dbName: String): Map[Int, List[Int]] = io.Source.fromFile("lib/prolog/"+dbName).getLines.toList.map(line => (line.take(9).toInt, line.drop(10).take(9).toInt)).groupBy(_._1).map(e => e._1 -> e._2.map(_._2)).withDefaultValue(List[Int]())
    val wnDbs = List("hyp","sim","ins","mm","ms","mp","at").map(db => db -> getMapFromwn(s"wn_$db.db")).toMap
    val stoplist = io.Source.fromFile(folder+"stoplist.db").getLines.toSet
    
    def getWeights: Map[String, Double] = {
      val file = io.Source.fromFile(folder+"weights")
      val out = file.getLines.toList.map(_.split(" ")).map(e => (e(0), e(1).toDouble)).toMap
      file.close
      out
    }
    
    def keywords(in: String, count: Int = 3): Option[List[String]] = {
      try {
        val scores = HashMap[String, Double]()
        def addToScore(str: String, add: Double) {
          if(!stoplist.contains(str)) scores(str) = (scores.getOrElse(str, 0.0)+add)
        }
        
        val words = in.split(" ").toList flatMap {
          case wordReg(prefix,word,suffix) => 
            if(word.size >= 3) {
              preprocess(word) filter { w => wn_sByWord contains w } flatMap { w =>
                if(!(stoplist contains w)) {
                  addToScore(w, 1)
                  wn_sByWord(w)
                } else {
                  Nil
                }
              }
            } else {
              Nil
            }
          case _ => Nil
        } //filter (e => e._3 == 'v' || e._3 == 'n')
        
        val weights = getWeights
        
        for(db <- wnDbs; word <- words; wId <- db._2(word._1); relatedWord <- wn_sById(wId)) {
          addToScore(relatedWord._2, weights(db._1))
        }
        
        val out = (scores.toList.sortWith(_._2 > _._2).take(count+30).filterNot(e =>
          scores.exists(a => 
            (a._1 != e._1 && a._2 >= e._2 && a._1.size <= e._1.size) && 
            (a._1.startsWith(e._1.take(4)) || (e._1 contains a._1))
          )// take out similar words
        ).take(count).map(_._1)) //sort and convert to string
        
        if(out.size > 0) Some(out) else None
      } catch {
        case e: Exception => e.printStackTrace; None
      }
    }
  }
  

  /// Store based on bash :) #softwareanarchitecture
  // talk about leaky abstractions...
  // also take it, or leave it :)
  object Store { def apply(file: String): Store = new Store(file) }
  class Store(file: String, keyFormat: String="""([-_a-zA-Z0-9]{1,16})""") {
    import sys.process._
    def isKey(s: String): Boolean = s matches keyFormat
    def +=(k: String, v: String = null) = (Seq("echo", if(v != null) k+" "+v else k) #>> new File(file)).!!
    def -=(k: String) = Seq("sed", "-i", s"""/^$k$$\\|^$k[ ].*$$/d""", file).!!
    def ?(k: String) = Seq("sed", "-n", s"""/^$k$$\\|^$k[ ].*$$/p""", file).!! split "\n" filter { _.size > 0 } map { res => res.substring(min(res.length,k.length+1)) } toList
    def * = Seq("cat", file).!! split "\n" filter { _.size > 0 } map { res => 
      val sep = res.indexOf(" ")
      if(sep == -1) (res, null) else (res.substring(0, sep), res.substring(sep+1))
    } toList

    def ?-(key: String) = {
      val out = ?(key)
      this -= key
      out
    }
  }
}
