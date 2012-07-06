package haibot

import collection.mutable._
import java.io._
import java.net._
import math._
import scala.util.matching._
import scala.util.Random._
import scala.io.Source._

object Utils {
    /// Pimped types
    class PimpString(s:String) {
        // TODO: containsPercent
        def containsAny(strs:String*)   = strs.foldLeft(false)((acc,str) => acc || s.contains(str))
        def startsWithAny(strs:String*) = strs.foldLeft(false)((acc,str) => acc || s.startsWith(str))
        def endsWithAny(strs:String*)   = strs.foldLeft(false)((acc,str) => acc || s.endsWith(str))
        def sentences = s.split("[.!?]+") // TODO: http://stackoverflow.com/questions/2687012/split-string-into-sentences-based-on-periods
        def makeEasy = s.toLowerCase.map(a=>("čćžšđ".zip("cczsd").toMap).getOrElse(a, a))
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
    }
    implicit def String2Pimp(s:String) = new PimpString(s)

    /// Some regexes
    object Regex {
        lazy val URL = """(?i)\b((?:[a-z][\w-]+:(?:/{1,3}|[a-z0-9%])|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'".,<>?«»“”‘’]))""".r
        lazy val Date = (
            """([0-9]{2}[/.][0-9]{2}[/.][0-9]{2,4})""" + // 00/00/yyyy
            """|([A-Z][a-z]{2,10} [0-9]{1,2}, [0-9]{4})""" + // meetup May 30, 2012
            """|([0-9]{1,2} [A-Z][a-z]{2,10} [0-9]{4})""" + // facebook 7 July 2012
            "").r
    }

    /// Ya, rly
    object Memes {
        val NO_U = """http://bit.ly/yaJI5L"""
        val oh_you = """http://bit.ly/MvTwUG"""
        val so_fluffy = """http://bit.ly/MG5Hfx"""
        val it_was_you = """http://bit.ly/zg0jQt"""
    }
    
    /// Time functions in ms
    var timeDivisor = 1000000L
    def now = (System.nanoTime()/timeDivisor).toInt
    def since(time:Int):Int = now-time
    private val sinceTimes = HashMap[Int,Int]()
    def since(timeRef:AnyRef):Int = {
        val time = sinceTimes.getOrElseUpdate(timeRef.hashCode, 0)
        val nnow = now
        sinceTimes(timeRef.hashCode) = nnow
        
        nnow-time
    }    
    def time(func: => Unit) = {
        val startTime = now
        func
        now-startTime
    }
    
    /// Wordnet stuff - also in bash, and on prolog data
    object WordNet {
        lazy val wn_s = fromFile("prolog/wn_s.db").getLines.toList

        //These take a while at startup... make them lazy if it bothers you
        val wn_sAll = fromFile("prolog/wn_s.db").getLines.toList.map(_.split(",")).map(e=> (e(0).toInt,e(2).init.tail.replaceAll("''","'"),e.last.toInt))
        val wn_sById = wn_sAll.groupBy(_._1)
        val wn_sByWord = wn_sAll.groupBy(_._2)
        
        def synonym(in:String*):List[String] = {
            in.map(str=> {
                val (prefix,suffix) = (
                    str.takeWhile(c=> !(c>='a' && c<='z')),
                    str.reverse.takeWhile(c=> !(c>='a' && c<='z')).reverse
                )
                if(prefix.size+suffix.size >= str.size-3 || !(str matches """[a-zA-Z0-9 .'/-]+""" )) {
                    str
                } else {
                    val tmpStr = str.substring(prefix.size, str.size-suffix.size)
                    if(wn_sByWord contains tmpStr) {
                        var syns = wn_sByWord(tmpStr).map(_._1).distinct.flatMap(e=> wn_sById(e).map(_._2))
                        syns = syns.filter(e=> e!=tmpStr && e.split(" ").forall(_.size>3))
                        if(syns.size > 0)
                            prefix+syns(nextInt(syns.size))+suffix
                        else
                            str
                    } else {
                        str
                    }
                }
            }).toList
        }
        
        def rephrase(s:String):String = synonym(s.split(" "):_*).mkString(" ")

        lazy val wn_hyp = fromFile("prolog/wn_hyp.db").getLines.toList.map(e=> (e.take(9).toInt, e.drop(10).take(9).toInt))
        lazy val wn_mm = fromFile("prolog/wn_mm.db").getLines.toList.map(e=> (e.take(9).toInt, e.drop(10).take(9).toInt))
        lazy val wn_mp = fromFile("prolog/wn_mp.db").getLines.toList.map(e=> (e.take(9).toInt, e.drop(10).take(9).toInt))

        def hypernym(in:String*):String = {
            println(in)
            val tasks = in.flatMap(str=> {
                val (prefix,suffix) = (
                    str.takeWhile(c=> !(c>='a' && c<='z')),
                    str.reverse.takeWhile(c=> !(c>='a' && c<='z')).reverse
                )
                if(prefix.size+suffix.size >= str.size-2 || !(str matches """[a-zA-Z0-9 .'/-]+""" )) 
                    Nil
                else 
                    List(str.substring(prefix.size, str.size-suffix.size))
            }).toList
    
            //prepare query
            val query = tasks.distinct.map(e=> e+"'")
            
            println(query)
            
            //query db for ids
            val ids = wn_s
                .filter(e=> e.substring(e.indexOf("'")+1).startsWithAny(query:_*))
                .map(e=> e.split(",")(0).toInt)
                .toSet
                
            println(ids)
            
            val scores = HashMap[Int, Double]()
            def score(id:Int,add:Double) = scores(id) = (scores.getOrElse(id, 0.0)+add)
            
            ids.foreach(e=> score(e,0.2))
            wn_hyp.foreach(e=> if(ids contains e._1) score(e._2,1.3) else if(ids contains e._2) score(e._1,0.1))
             wn_mm.foreach(e=> if(ids contains e._1) score(e._2,0.3) else if(ids contains e._2) score(e._1,0.1))
             wn_mp.foreach(e=> if(ids contains e._1) score(e._2,0.3) else if(ids contains e._2) score(e._1,0.1))
            
            val hadId = HashSet[Int]()
            val idds = scores.toList.sortWith(_._2 > _._2).take(100)
            val ids2 = idds.map(_._1).toSet
            
            val shitlist = Set[String]()//"move change alter modify person individual someone somebody mortal soul be".split(" ").toSet
            var out = wn_s
                .filter(e=> ids2 contains e.take(9).toInt)
                .map(_.split(","))
                .sortWith((a,b)=> 
                    idds.find(_._1 == a(0).toInt).get._2 + (a.last.toInt+1)*0.5 > idds.find(_._1 == b(0).toInt).get._2 + (b.last.toInt+1)*0.5
                )
                .filter(e=>{ true
                    if(hadId contains e(0).toInt) false else { hadId += e(0).toInt; true} 
                })
                .map(e=> e(2).tail.init + idds.find(_._1 == e(0).toInt).get._2.toString.take(3))
                .filterNot(e=> shitlist contains e.dropRight(3)).distinct
            out = out
                .filterNot(e=> out.exists(a=> a.startsWith(e.take(4)) && a.length < e.length ))
                .distinct.take(20)
            //out = out.filterNot(e=> out.exists(a=> a.startsWith(e.take(4)) && a.length < e.length )).distinct.take(5)
            out.mkString(", ")
        }

        def context(s:String):String = hypernym(s.split(" "):_*)
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
