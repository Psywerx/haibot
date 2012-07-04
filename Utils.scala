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
        
        abstract class SynComp
        case class Prep(prefix:String,suffix:String,word:String) extends SynComp
        case class ResList(s:List[String]) extends SynComp
        
        //TODO: settings: min length, prevent same, etc
        def synonyms(in:String*):List[String] = {
            var comp = in.map(str=> {
                var (prefix,suffix) = ( //TODO: handle ThIs kiNd of stuff "and,this"
                    str.takeWhile(c=> !(c>='a' && c<='z')),
                    str.reverse.takeWhile(c=> !(c>='a' && c<='z')).reverse
                )
                if(prefix.size+suffix.size >= str.size-2) 
                    ResList(List(str))
                else 
                    Prep(prefix,suffix,str.substring(prefix.size, str.size-suffix.size))
            })
            
            //prepare query
            var query = comp.flatMap({
                case Prep(prefix,suffix,word) => List(word)
                case _ => Nil
            }).distinct.map(e=> "'"+e+"'")
            
            //query db for words
            var ids = ListBuffer[String]()
            var results = wn_sFile
                .filter(_.containsAny(query:_*))
                .map(e=> {
                    val out = e.split(",")
                    ids += out(0)+","
                    out
                })
                .groupBy(_(2)) // word -> s(...)
            
            //query db for syn ids
            var results2 = wn_sFile
                .filter(_.startsWithAny(ids:_*))//TODO: with prefix-tree
                .map(str => str.split(","))
                .groupBy(_(0)) // synsetID -> s(...)
            
            return comp.map({
                case Prep(prefix,suffix,word) =>
                    try {
                        results("'"+word+"'").flatMap(res=> 
                            results2(res(0)).map(e=> prefix+e(2).tail.init+suffix)
                            .filter(_.split(" ").forall(_.size>=2))
                        )
                    } catch {
                        case e:Exception => List(prefix+word+suffix)
                    }
                case ResList(a) => a
                case _ => List("???")
            }).map(words=> shuffle(words).toList(0)).toList

/*            return comp.map(sc=>
                sc match { 
                    case Prep(prefix,suffix,str) =>
                        try {
                            results(str).flatMap(res=> results2(res._1).map(_._3)).map(w=> prefix+w+suffix).filter(_.split(" ").forall(_.size>=3))
                        } catch {
                            case _ => List(prefix+str+suffix)
                        }
                    case ResList(a) => a
                    case _ => List("?")
                }
            ).map(words=> shuffle(words).toList(0)).toList
            //return comp.map(words=> shuffle(words._1).toList(0))*/
            /*
            
            //TOI: if(s.size<=3 || nextFloat<0.2) return in
            
            try {
                var wn_s = sed(s"'$s'", wn_sPath).map(str => {
                    val wn_sReg(_SynsetID,_WordNumber,_Word,_Type,_Sense,_Count) = str;
                    (_SynsetID,_WordNumber,_Word,_Type,_Sense,_Count)
                })
                
                if(wn_s.size==0) return in //no luck
                
                //val bestSynsets = wn_s.sortWith(_._6.toInt > _._6.toInt)
                //val bestSynsetID = shuffle(bestSynsets.filter(_._6.toInt >= bestSynsets(0)._6.toInt-2)).toList(0)._1
                //wn_s = sed(s"""$bestSynsetID,""", wn_sPath)...
                
                wn_s = fromFile(wn_sPath).getLines.toList.filter(_.containsAny(wn_s.map(e=> e._1):_*))
                    .map(str => {
                        val wn_sReg(_SynsetID,_WordNumber,_Word,_Type,_Sense,_Count) = str;                    
                        (_SynsetID,_WordNumber,_Word,_Type,_Sense,_Count)
                    }).filter(_._3!=s).sortWith(_._6.toInt > _._6.toInt)
                if(wn_s.size==0) return in //no luck
                var sum = wn_s.map(_._6.toInt*3+1).sum // weighted random
                var res = nextInt(sum)
                prefix+wn_s.find(syn=> {
                    res -= syn._6.toInt*3+1
                    (res<=0)
                }).get._3+postfix
            } catch {
                case _ => in
            }*/
        }
        
        //def rephrase(s:String):String = s.split(" ").map(synonym).mkString(" ")
        def rephrase(s:String):String = synonyms(s.split(" "):_*).mkString(" ")
    }

    /// some things just shouldn't exist
    object sed {
        import sys.process._
        def apply(s:String, file:String) = 
            Seq("sed","-n", s"/$s/p",file).!!.split("\n").filter(_!="").toList
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
