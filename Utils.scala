package haibot

import collection.mutable.{HashSet,ListBuffer,HashMap}
import java.io._
import java.net._
import math._
import scala.util.matching._
import scala.util.Random._

object Utils {
  val folder = "utils/"
  //TODO: pack each thing up with imports it needs, so you can copy-paste and such

  def withAlternative[T](func: => T, alternative: => T ): T = try { func } catch { case _: Throwable => alternative}
  def withExit[T](func: => T, exit: => Any = { }): T = try { func } catch { case _: Throwable => exit; sys.exit(-1) }
  def tryOption[T](func: => T): Option[T] = try { Some(func) } catch { case _: Throwable => None }

  /// Pimped types
  implicit class PimpString(val s:String) { 
    def replaceAll(m:(String,String)*):String = m.foldLeft(s)((out,rep)=> out.replaceAll(rep._1,rep._2))
    // TODO: containsPercent:Double for fuzzy reasoning
    def containsAny(strs:String*)   = strs.foldLeft(false)((acc,str) => acc || s.contains(str))
    def startsWithAny(strs:String*) = strs.foldLeft(false)((acc,str) => acc || s.startsWith(str))
    def endsWithAny(strs:String*)   = strs.foldLeft(false)((acc,str) => acc || s.endsWith(str))
    def sentences = s.split("[.!?]+") // TODO: http://stackoverflow.com/questions/2687012/split-string-into-sentences-based-on-periods
    def makeEasy = s.toLowerCase.map(a=>("čćžšđ".zip("cczsd").toMap).getOrElse(a, a)).replaceAll(", ", " ")
    def maybe = if(0.5.prob) s else ""
    def findAll(r:Regex) = r.findAllIn(s).toList
    def removeAll(rem:String) = s.filterNot(rem contains _)
    def distance(s2:String):Int = distance(s,s2)
    private def distance(s1:String,s2:String): Int = (s1,s2) match {
      case ("",s2) => s2.size
      case (s1,"") => s1.size
      case (s1,s2) => 
        if(s1.last == s2.last) 
          distance(s1.init, s2.init)
        else
          Seq(
             1 + distance(s1.init, s2),
             1 + distance(s1, s2.init),
             1 + distance(s1.init, s2.init)
          ).min
    }
    def matches(r:util.matching.Regex) = s.matches(r.toString)
  }
  
  implicit class MaybeSI(val sc: StringContext) extends AnyVal { def maybe(args:Any*):String = sc.parts.iterator.mkString("").maybe }
  implicit class PimpInt(val i:Int) extends AnyVal { def ~(j:Int) = nextInt(j-i+1)+i }

  // hashmap with default value
  /*class HashMapDef[A,B](defVal:B) extends HashMap[A,B] {
    override def default(key:A) = defVal
  }
  implicit class HashMap2Def[A,B](hm:HashMap[A,B]) { //TODO: copy values or something
    def withDefaultValue(defVal:B) = new HashMapDef[A,B](defVal)
  }*/
  
  implicit class Seqs[A](val s:Seq[A]) { 
    def random = s(nextInt(s.size)) 
    def randomOption = if(s.size > 0) Some(s(nextInt(s.size))) else None
  }

  implicit class D(val d:Double) { def prob = nextDouble<d } //0.5.prob #syntaxabuse
  implicit class F(val f:Float) { def prob = nextFloat<f }
  implicit class I(val i:Int) { def isBetween(min:Int,max:Int) = i >= min && i <= max}

  /// Some regexes
  object Regex {
    val URL = """(?i)\b((?:[a-z][\w-]+:(?:/{1,3}|[a-z0-9%])|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'".,<>?«»“”‘’]))""".r
    
    val tweet = "https?://(?:www\\.)?twitter\\.com/.*/status(?:es)?/([0-9]+).*".r
    
    // do something like this http://stackoverflow.com/questions/3389348/parse-any-date-in-java
    val Date = (
      """([0-9]{2}[/.][0-9]{2}[/.][0-9]{2,4})""" + // 00/00/yyyy
      """|([A-Z][a-z]{2,10} [0-9]{1,2}, [0-9]{4})""" + // meetup May 30, 2012
      """|([0-9]{1,2} [A-Z][a-z]{2,10} [0-9]{4})""" + // facebook 7 July 2012
      "").r
  }
  
  object Date {
    //wish I knew a better way.
    import java.util.Date
    import java.text._
    
    private val dateFormats = List[(util.matching.Regex, SimpleDateFormat)](
      ("""\d{1,2}[.]\d{1,2}[.]\d{4}""".r, new SimpleDateFormat("dd.MM.yyyy")),
      ("""\d{1,2}-\d{1,2}-\d{4}""".r, new SimpleDateFormat("dd-MM-yyyy")),
      ("""\d{4}-\d{1,2}-\d{1,2}""".r, new SimpleDateFormat("yyyy-MM-dd")),
      ("""\d{1,2}/\d{1,2}/\d{4}""".r, new SimpleDateFormat("MM/dd/yyyy")),
      ("""\d{4}/\d{1,2}/\d{1,2}""".r, new SimpleDateFormat("yyyy/MM/dd")),
      ("""\d{1,2}\s[a-z]{3}\s\d{4}""".r, new SimpleDateFormat("dd MMM yyyy")),
      ("""\d{1,2}\s[a-z]{4,}\s\d{4}""".r, new SimpleDateFormat("dd MMMM yyyy")),
      ("""\d{1,2}[.]\s[a-z]{4,}\s\d{4}""".r, new SimpleDateFormat("dd. MMMM yyyy")),
      ("""\d{1,2}[.]\d{1,2}[.]\d{4}\s\d{1,2}:\d{2}""".r, new SimpleDateFormat("dd-MM-yyyy HH:mm")),
      ("""\d{1,2}-\d{1,2}-\d{4}\s\d{1,2}:\d{2}""".r, new SimpleDateFormat("dd-MM-yyyy HH:mm")),
      ("""\d{4}-\d{1,2}-\d{1,2}\s\d{1,2}:\d{2}""".r, new SimpleDateFormat("yyyy-MM-dd HH:mm")),
      ("""\d{1,2}/\d{1,2}/\d{4}\s\d{1,2}:\d{2}""".r, new SimpleDateFormat("MM/dd/yyyy HH:mm")),
      ("""\d{4}/\d{1,2}/\d{1,2}\s\d{1,2}:\d{2}""".r, new SimpleDateFormat("yyyy/MM/dd HH:mm")),
      ("""\d{1,2}\s[a-z]{3}\s\d{4}\s\d{1,2}:\d{2}""".r, new SimpleDateFormat("dd MMM yyyy HH:mm")),
      ("""\d{1,2}\s[a-z]{4,}\s\d{4}\s\d{1,2}:\d{2}""".r, new SimpleDateFormat("dd MMMM yyyy HH:mm")),
      ("""\d{1,2}[.]\s[a-z]{4,}\s\d{4}\s\d{1,2}:\d{2}""".r, new SimpleDateFormat("dd. MMMM yyyy HH:mm"))
    )

    private def deLocale(dateStr:String) = dateStr
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

    def getFutureDates(text:String): List[Date] = {
      val now = new Date
      dateFormats.flatMap { case (regex, format) => 
        text.findAll(regex)
          .flatMap(dateStr => tryOption(format.parse(deLocale(dateStr))))
          .filter(date => date.after(now)) // only future dates
      }.distinct.sortWith((a,b) => b.after(a))
    }
    
    private val timeDivisor = 1000000L*1000L
    def since(time: Int): Int = now-time
    def now = (System.nanoTime()/timeDivisor).toInt
    def time(func: => Unit) = {
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
    private def pad(i: Int, p: Int = 2) = "0"*(p-i.toString.size)+i.toString
    def getSinceString(time:Int):String = {
        val secs = since(time)
        val (days, hours, minutes, sec) = ((secs/60/60/24), pad((secs/60/60)%24), pad((secs/60)%60), pad((secs)%60))
        
        ((days match {
          case 0 => ""
          case 1 => s"$days day "
          case _ => s"$days days "
        }) + (if(secs < 90) s"$secs seconds" else s"$hours:$minutes"))
      }    
  }
  
  /// Ya, rly
  object Memes {
    val NO_U = """http://bit.ly/yaJI5L"""
    val oh_you = """http://bit.ly/MvTwUG"""
    val so_fluffy = """http://bit.ly/MG5Hfx"""
    val it_was_you = """http://bit.ly/zg0jQt"""
  }

  object Net {
    import de.l3s.boilerpipe.extractors._
    import java.net._
    val extractor = KeepEverythingExtractor.INSTANCE
    
    def badExts = io.Source.fromFile(folder+"badexts.db").getLines.toBuffer
    def scrapeURLs(urls:String*):String = { //TODO: ni se mi dal :)
      urls.map(
        _.findAll(Regex.URL).map(url => 
          try {
            if(!url.endsWithAny(badExts:_*)) 
              extractor.getText(new URL(url))
            else 
              ""
          } catch { 
            case e: Exception => 
              e.printStackTrace()
              "" 
          }
        ).fold("")(_+" "+_)
      ).fold("")(_+" "+_)
    }
  }

  /// Wordnet stuff - also in bash, and on prolog data
  object WordNet {
    //These take a while at startup... make them lazy if it bothers you
    //format is (id,word,type)... I should probably keep type
    type wn_s = (Int,String,Char)
    val wn_sAll = io.Source.fromFile("lib/prolog/wn_s.db").getLines.toList.map(_.split(",")).map(e=> (e(0).toInt,e(2).init.tail.replaceAll("''","'"),e(3)(0)))
    val wn_sById = wn_sAll.groupBy(_._1).withDefaultValue(List[wn_s]())
    val wn_sByWord = wn_sAll.groupBy(_._2).withDefaultValue(List[wn_s]())
    
    val wordReg = """([^a-zA-Z0-9 .'/-]*)([a-zA-Z0-9 .'/-]{3,})([^a-zA-Z0-9 .'/-]*)""".r
    val capitals = """[A-Z][a-zA-Z0-9 .'/-]*"""
    val caps = """[A-Z0-9 .'/-]+"""
    val lower = """[a-z0-9 .'/-]+"""
    
    def synonyms(strs:String*):List[String] = strs.map(str=> synonym(str)).toList
    def synonym(str:String):String = str match {
      case wordReg(prefix,word,suffix) => 
        val (capital, allcaps, alllower) = (word matches capitals, word matches caps, word matches lower)
        val part = if(!(allcaps || alllower)) {
          val upper = word.count(e=> e.toString==e.toString.toUpperCase).toFloat
          val lower = word.count(e=> e.toString==e.toString.toLowerCase).toFloat
          upper/(upper+lower)
        } else {
          0f
        }

        val syns = wn_sByWord(word.toLowerCase) // find word
          .map(_._1).distinct.flatMap(e=> wn_sById(e).map(_._2)) // query all synonyms by id
          .filter(e=> e!=word && e.split(" ").forall(_.size>=4)).distinct // filter probably useless ones
        if(syns.size > 0) {
          var out = syns(nextInt(syns.size))          
          if(out.toLowerCase == word.toLowerCase)
            out = word else
          if(alllower) 
            out = out.toLowerCase else
          if(allcaps) 
            out = out.toUpperCase else
          if(capital && part<0.25) 
            out = out.head.toString.toUpperCase+out.tail else 
          if(out matches lower) {
            out = out.map(e=> if(nextFloat<part) e.toString.toUpperCase.head else e.toString.toLowerCase.head)
            if(capital) out = out.head.toString.toUpperCase+out.tail
          }
          
          prefix+out+suffix
        } else {
          str
        }
      case _ => str
    }
    def rephrase(s:String):String = synonyms(s.split(" "):_*).mkString(" ")
    
    
    def stem(w:String):List[String] = // bad excuse for a stemmer :)
      (List(w) ++ 
        w.split("-")
          .flatMap(w=> List(w, w.replaceAll("ability$", "able")))
          .flatMap(w=> List(w, w.replaceAll("s$|er$|est$|ed$|ing$|d$", "")))
          //.flatMap(w=> List(w, w.replaceAll("^un|^im|^in", "")))
          .filter(_.size>=4)
      ).distinct
      
    def preprocess(w:String):List[String] = 
      stem(w).flatMap(e=> List(e, e.toLowerCase)).distinct
    
    def getMapFromwn(dbName:String) = io.Source.fromFile("lib/prolog/"+dbName).getLines.toList.map(e=> (e.take(9).toInt, e.drop(10).take(9).toInt)).groupBy(_._1).map(e=> e._1 -> e._2.map(_._2)).withDefaultValue(List[Int]())
    val wn = List("hyp","sim","ins","mm","ms","mp","at").map(e=> e -> getMapFromwn(s"wn_$e.db")).toMap
    val stoplist = io.Source.fromFile(folder+"stoplist.db").getLines.toSet ++ Set("answer", "test", "issue", "start", "publication")
    
    var weights = getWeights
    def getWeights = {
      val file = io.Source.fromFile(folder+"weights")
      val out = file.getLines.toList.map(_.split(" ")).map(e=> (e(0), e(1).toFloat)).toMap
      file.close
      out
    }
    
    def context(in:String):String = {
      try {
        val scores = HashMap[String, Double]()
        def score(str:String,add:Double) = if(!(stoplist contains str)) scores(str) = (scores.getOrElse(str, 0.0)+add)

        val words = in.split(" ").toList.flatMap({
          case wordReg(prefix,word,suffix) => 
            if(word.size >= 3) {
              preprocess(word).filter(w=>wn_sByWord contains w).flatMap(w=> {
                if(!(stoplist contains w)) {
                  score(w,1)
                  wn_sByWord(w)
                } else {
                  Nil
                }
              })
            } else {
              Nil
            }
          case _ => Nil
        }).filter(e=> e._3=='v' || e._3=='n')
        
        getWeights
        
        wn.foreach(wndb=> 
          words.foreach(elt=>           
            wndb._2(elt._1).foreach(id2=> 
              wn_sById(id2).foreach(elt=> 
                score(elt._2, weights(wndb._1))
              )
            )
          )
        )
        
        scores.toList.sortWith(_._2 > _._2).take(30).filterNot(e=> 
          scores.exists(a=> 
            (a._1!=e._1 && a._2 >= e._2 && a._1.size <= e._1.size) && 
            (a._1.startsWith(e._1.take(4)) || (e._1 contains a._1))
          )//take out similars
        ).map(e=> e._1).take(3).mkString(", ") //sort and convert to string
      } catch {
        case e:Exception => e.printStackTrace; ""
      }
    }
  }
  

  /// Store based on bash :) #softwareanarchitecture
  // talk about leaky abstractions...
  // also take it, or leave it :)
  object Store { def apply(file:String) = new Store(file) }
  class Store(file:String, keyFormat:String="""([-_a-zA-Z0-9]{1,16})""") {
    import sys.process._
    def isKey(s:String) = s matches keyFormat
    def +(k:String, v:String=null) = (Seq("echo", if(v!=null) k+" "+v else k) #>> new File(file)).!!
    def -(k:String) = Seq("sed", "-i", s"""/^$k$$\\|^$k[ ].*$$/d""", file).!!
    def ?(k:String) = Seq("sed", "-n", s"""/^$k$$\\|^$k[ ].*$$/p""", file).!!.split("\n").filter(_!="").map(res=> res.substring(min(res.length,k.length+1))).toList
    def * = Seq("cat", file).!!.split("\n").filter(_!="").map(res=> {
      val sep = res.indexOf(" ")
      if(sep == -1) (res, null) else (res.substring(0,sep), res.substring(sep+1))      
    }).toList

    def ?-(key:String) = {
      val out = ?(key)
      this.-(key)
      out
    }
  }
}
