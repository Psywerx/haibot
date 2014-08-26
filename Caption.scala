package org.psywerx

import scala.util.parsing.combinator._
import scala.util.Random._
import scala.language.postfixOps
import java.lang.StringBuilder
import org.psywerx.util.Seqs

//TODO: oh god... sloppy implementation, also, needs multilevel parsing
final object Caption extends RegexParsers {
  override val skipWhitespace = false
  
  val text = ".+?".r
  
  def group = """[\[][^\]]*[\]]([0-9](-[0-9])?)?""".r ^^ { g => 
    val (str, repeats) = {
      val last1 = g.takeRight(1)
      val last3 = g.takeRight(3)
      if(last3 matches "[0-9]-[0-9]") {
        val (n1, n2) = (last3.take(1).toInt, last3.takeRight(1).toInt)
        (g.dropRight(3), nextInt(n2-n1 + 1) + n1)
      } else if(last1 matches "[0-9]") {
        (g.dropRight(1), nextInt(last1.toInt) + 1)
      } else {
        (g, 1)
      }
    }
    val option = str.drop(1).dropRight(1).split('|').toSeq.random
    
    option * repeats
  }
  def groupMaybe = """[{][^}]*[}]([0-9](-[0-9])?)?""".r ^^ { g => 
    val (str, repeats) = {
      val last1 = g.takeRight(1)
      val last3 = g.takeRight(3)
      if(last3 matches "[0-9]-[0-9]") {
        val (n1, n2) = (last3.take(1).toInt, last3.takeRight(1).toInt)
        var repeats = nextInt(n2-n1 + 2) + n1
        if(repeats > n2) repeats = 0
        (g.dropRight(3), repeats)
      } else if(last1 matches "[0-9]") {
        (g.dropRight(1), nextInt(last1.toInt) + 1)
      } else {
        (g, 1)
      }
    }
    val option = str.drop(1).dropRight(1).split('|').toSeq.random
    
    option * repeats
  }
  def quote = """@@@.*?@@@""".r ^^ { g => g.drop(3).dropRight(3) }
  
  def apply(s: String): String = 
    parse((group | groupMaybe | quote | text)*, s).get.mkString
      .replaceAll("\\s+", " ").trim
      .replace("%%%%pipe%%%%", "|")
      .replace("%%%lbrack%%%", "[")
      .replace("%%%rbrack%%%", "]")
      .replace("%%%lbrace%%%", "{")
      .replace("%%%rbrace%%%", "}")
      .replace("%%%atsign%%%", "@")
      .replace(" ,", ",")
      .replace(" .", ".")
      .replace(" !", "!")
      .replace(" ?", "?")
      .trim

  // standard string interpolation + Caption parsing
  implicit class C(val sc: StringContext) extends AnyVal {
    import scala.StringContext._
    import sc.parts

    def checkLengths(args: Seq[Any]): Unit =
      if (parts.length != args.length + 1)
        throw new IllegalArgumentException("wrong number of arguments for interpolated string")
        
    def c(args: Any*): String = Caption(standardInterpolator(treatEscapes, args))

    def standardInterpolator(process: String => String, args: Seq[Any]): String = {
      checkLengths(args)
      val pi = parts.iterator
      val ai = args.iterator
      val builder = new StringBuilder(process(pi.next()))
      while (ai.hasNext) {
        val arg = 
          ai.next.toString
            .replace("|", "%%%%pipe%%%%")
            .replace("[", "%%%lbrack%%%")
            .replace("]", "%%%rbrack%%%")
            .replace("{", "%%%lbrace%%%")
            .replace("}", "%%%rbrace%%%")
            .replace("@", "%%%atsign%%%")
            
        builder append ("@@@"+arg+"@@@")
        val part = pi.next()
        builder append process(part)
      }
      builder.toString
    }
  }
  
  def test() = {
    val str1 = "hello"
    val str2 = "hello [nope nope@ nope] @@@{cheeki breeki} @ ayy lmao @"
    val str3 = """https://www.google.si/maps/dir/August-Bebel-Stra%C3%9Fe,+Bielefeld,+Germany/August-Bebel-Stra%C3%9Fe+8,+33602+Bielefeld,+Germany/@52.0306075,8.5365767,16z/data=!4m14!4m13!1m5!1m1!1s0x47ba3d0fcb66b3a3:0xd3692e498e1463e7!2m2!1d8.5397073!2d52.0223094!1m5!1m1!1s0x47ba3d6d57e68a65:0x3339d46a21206dce!2m2!1d8.539759!2d52.0314863!3e2?hl=en"""
    
    def cases() = Vector(
      (c"[hello|hi], welcome to #psywerx", 
        Set("hello, welcome to #psywerx", "hi, welcome to #psywerx")),
      (c"{hello|hi}, welcome to #psywerx", 
        Set("hello, welcome to #psywerx", "hi, welcome to #psywerx", ", welcome to #psywerx")),
      (c"welcome to [something|something] {something|something} #psywerx", 
        Set("welcome to something something #psywerx", "welcome to something #psywerx")),
      (c"[hello|hi], welcome to #psywerx, $str1", 
        Set("hello, welcome to #psywerx, "+str1, "hi, welcome to #psywerx, "+str1)),
      (c"[hello|hi], welcome to #psywerx, $str2", 
        Set("hello, welcome to #psywerx, "+str2, "hi, welcome to #psywerx, "+str2)),
      (c"[hello], welcome to #psywerx, $str2 lol [wat|cat]", 
        Set("hello, welcome to #psywerx, "+str2+" lol wat", "hello, welcome to #psywerx, "+str2+" lol cat")),
      (c"[hello], welcome to #psywerx, $str3 lol [wat|cat]", 
        Set("hello, welcome to #psywerx, "+str3+" lol wat", "hello, welcome to #psywerx, "+str3+" lol cat")),
      (c"[hi]3-3, welcome to #psywerx", 
        Set("hihihi, welcome to #psywerx")),
      (c"[hi]2-3, welcome to #psywerx", 
        Set("hihi, welcome to #psywerx", "hihihi, welcome to #psywerx")),
      (c"{hi}2-3, welcome to #psywerx", 
        Set(", welcome to #psywerx", "hihi, welcome to #psywerx", "hihihi, welcome to #psywerx")),
      (c"{}2-3[]5-7, welcome to #psywerx", 
        Set(", welcome to #psywerx")),
      (c"{h}0-1[i]0-1, welcome to #psywerx", 
        Set(", welcome to #psywerx", "h, welcome to #psywerx", "hi, welcome to #psywerx", "i, welcome to #psywerx")),
      (c"[hello|hi], we@@@@[l]{c}@o@@@@me to #psywerx", 
        Set("hello, we@[l]{c}@o@me to #psywerx", "hi, we@[l]{c}@o@me to #psywerx"))
    )
        
    for(i <- 1 to 1000; (cap, resSet) <- cases(); if(!resSet.contains(cap))) {
      println
      println(cap)
      resSet.foreach(println)
    }
  }
}
