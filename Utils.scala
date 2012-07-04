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
        val wn_sPath = "prolog/wn_s.db"
        lazy val wn_sFile = fromFile(wn_sPath).getLines.toList
        //lazy val wn_sReg = """s\(([0-9]+),([0-9]+),'(.*?)',([a-z]+),([0-9]+),([0-9]+)\)\.""".r // s(100844254,3,'sex',n,1,14).
        
        abstract class SynTask
        case class Process(prefix:String,suffix:String,word:String) extends SynTask
        case class Alternatives(s:List[String]) extends SynTask
        case class Result(s:String) extends SynTask
        
        //TODO: settings: min length, prevent same, etc
        def synonyms(in:String*):List[String] = {
            val tasks = in.map(str=> {
                val (prefix,suffix) = ( //TODO: handle ThIs kiNd of stuff "and,this"
                    str.takeWhile(c=> !(c>='a' && c<='z')),
                    str.reverse.takeWhile(c=> !(c>='a' && c<='z')).reverse
                )
                if(prefix.size+suffix.size >= str.size-2 || !(str matches """[a-zA-Z0-9 .'/-]+""" )) 
                    Result(str)
                else 
                    Process(prefix,suffix,str.substring(prefix.size, str.size-suffix.size))
            }).toList
            
            //prepare query
            val query = tasks.flatMap({
                case Process(prefix,suffix,word) => List(word)
                case _ => Nil
            }).distinct.map(e=> e+"'")
            
            //query db for words
            val ids = HashSet[Int]()
            val results = wn_sFile
                .filter(e=> e.substring(e.indexOf("'")+1).startsWithAny(query:_*))
                .map(e=> {
                    val out = e.split(",")
                    ids += out(0).toInt
                    out
                })
                .groupBy(_(2)) // word -> s(...)
            
            //query db for syn ids
            val results2 = wn_sFile
                .filter(e=> ids contains e.take(9).toInt)
                .map(_.split(","))
                .groupBy(_(0)) // synsetID -> s(...)
            
            return tasks
                .map({
                    case Process(prefix,suffix,word) =>
                        try {
                            Alternatives(
                                results("'"+word+"'").flatMap(res=> 
                                    results2(res(0))
                                        .map(e=> prefix+e(2).tail.init.replaceAll("''","'")+suffix)
                                        .filter(_.split(" ").forall(_.size>=3))
                                )
                            )
                        } catch {
                            case _ => Result(prefix+word+suffix)
                        }
                    case st:SynTask => st
                })
                .map({
                    case Result(r) => r
                    case Alternatives(rl) => rl(nextInt(rl.size))
                })
        }
        
        def rephrase(s:String):String = synonyms(s.split(" "):_*).mkString(" ")
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
