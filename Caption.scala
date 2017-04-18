package org.psywerx

import scala.util.parsing.combinator._
import scala.util.Random._
import scala.language.postfixOps
import java.lang.StringBuilder
import org.psywerx.util.SeqImplicits

//TODO: needs multilevel parsing
final object Caption extends RegexParsers {
  override val skipWhitespace = false

  val text = ".+?".r
  val notBrack = """[^\[\]]""".r
  val notBrace = """[^{}]""".r
  def quote = """@@@.*?@@@""".r ^^ { g => g.drop(3).dropRight(3) }

  def brack = "[" ~ rep(quote | notBrack) ~ "]"
  def brace = "{" ~ rep(quote | notBrace) ~ "}"

  def group = ((brack | brace) ~ "([0-9](-[0-9])?)?".r) ^^ {
    case lB ~ options ~ rB ~ reps =>
      val repeats =
        (if (reps matches "[0-9]-[0-9]") {
          val (n1, n2) = (reps.take(1).toInt, reps.takeRight(1).toInt)
          lB match {
            case "[" =>
              nextInt(n2-n1 + 1) + n1
            case "{" =>
              var repeats = nextInt(n2-n1 + 2) + n1
              if (repeats > n2) repeats = 0
              repeats
          }
        } else if (reps matches "[0-9]") {
          lB match {
            case "[" => nextInt(reps.toInt) + 1
            case "{" => nextInt(reps.toInt + 1)
          }
        } else {
          1
        })

      val option = options.mkString.split('|').toSeq.random

      option * repeats
  }

  def apply(s: String): String =
    parseAll((group | quote | text)*, s).get.mkString
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
      val partsIter: Iterator[String] = parts.iterator
      val argsIter: Iterator[Any] = args.iterator
      val builder = new StringBuilder(process(partsIter.next()))
      while (argsIter.hasNext) {
        val arg =
          argsIter.next.toString
            .replace("|", "%%%%pipe%%%%")
            .replace("[", "%%%lbrack%%%")
            .replace("]", "%%%rbrack%%%")
            .replace("{", "%%%lbrace%%%")
            .replace("}", "%%%rbrace%%%")
            .replace("@", "%%%atsign%%%")

        builder append ("@@@"+arg+"@@@")
        val part = partsIter.next()
        builder append process(part)
      }
      builder.toString
    }
  }

  def test(): Unit = {
    val str1 = "hello"
    val str2 = "hello [nope nope@ nope] @@@{cheeki breeki} @ ayy @@@lmao @"
    val str3 = """https://www.google.si/maps/dir/August-Bebel-Stra%C3%9Fe,+Bielefeld,+Germany/August-Bebel-Stra%C3%9Fe+8,+33602+Bielefeld,+Germany/@52.0306075,8.5365767,16z/data=!4m14!4m13!1m5!1m1!1s0x47ba3d0fcb66b3a3:0xd3692e498e1463e7!2m2!1d8.5397073!2d52.0223094!1m5!1m1!1s0x47ba3d6d57e68a65:0x3339d46a21206dce!2m2!1d8.539759!2d52.0314863!3e2?hl=en"""
    val str4 = "hello [nope n{ope@ nope] @@@{che]eki breeki} @ ay}y {}{[][]}{][}{}@@@lmao @"

    def cases() = {
      val special = Seq("|", "[", "]", "{", "}", "@", "@@@", "%", "%%%%")
      val rand = Array.fill(100)(if (nextDouble > 0.8) scala.util.Random.nextPrintableChar.toString else special.random).mkString
      val cleanRand = special.foldLeft(rand)((acc, spec) => acc.replace(spec, ""))

      Vector(
        (c"[hello|hi], welcome to #psywerx", Set("hello, welcome to #psywerx", "hi, welcome to #psywerx")),
        (c"{hello|hi}, welcome to #psywerx",
          Set("hello, welcome to #psywerx", "hi, welcome to #psywerx", ", welcome to #psywerx")),
        (c"welcome to [something|something] {something|something} #psywerx",
          Set("welcome to something something #psywerx", "welcome to something #psywerx")),
        (c"[hello|hi], welcome to #psywerx, $str1",
          Set("hello, welcome to #psywerx, "+str1, "hi, welcome to #psywerx, "+str1)),
        (c"[hello|hi], welcome to #psywerx, $str2",
          Set("hello, welcome to #psywerx, "+str2, "hi, welcome to #psywerx, "+str2)),
        (c"[hello|hi], welcome to #psywerx, $str3",
          Set("hello, welcome to #psywerx, "+str3, "hi, welcome to #psywerx, "+str3)),
        (c"[hello|hi], welcome to #psywerx, $str4",
          Set("hello, welcome to #psywerx, "+str4, "hi, welcome to #psywerx, "+str4)),
        (c"[hello], welcome to #psywerx, $str2 lol [wat|cat]",
          Set("hello, welcome to #psywerx, "+str2+" lol wat", "hello, welcome to #psywerx, "+str2+" lol cat")),
        (c"[hello], welcome to #psywerx, $str3 lol [wat|cat]",
          Set("hello, welcome to #psywerx, "+str3+" lol wat", "hello, welcome to #psywerx, "+str3+" lol cat")),
        (c"[hi]3-3, welcome to #psywerx", Set("hihihi, welcome to #psywerx")),
        (c"[hi]2-3, welcome to #psywerx", Set("hihi, welcome to #psywerx", "hihihi, welcome to #psywerx")),
        (c"{hi}2-3, welcome to #psywerx",
          Set(", welcome to #psywerx", "hihi, welcome to #psywerx", "hihihi, welcome to #psywerx")),
        (c"{}2-3[]5-7, welcome to #psywerx",
          Set(", welcome to #psywerx")),
        (c"{h}0-1[i]0-1, welcome to #psywerx",
          Set(", welcome to #psywerx", "h, welcome to #psywerx", "hi, welcome to #psywerx", "i, welcome to #psywerx")),
        (c"[hello|hi], we@@@@[l]{c}@o@@@@me to #psywerx",
          Set("hello, we@[l]{c}@o@me to #psywerx", "hi, we@[l]{c}@o@me to #psywerx")),
        (c"this reaches ${str1} {when ${str2} return${str3}} [here]",
          Set("this reaches "+str1+" here", "this reaches "+str1+" when "+str2+" return"+str3+" here")),
        (c"{h}5", Set("hhhhh", "hhhh", "hhh",  "hh", "h", "")),
        (c"[h]5", Set("hhhhh", "hhhh", "hhh",  "hh", "h")),
        (c"{h}0-1", Set("h", "")),
        (c"[h]0-1", Set("h", "")),
        (c"{I swer on me[] mum}", Set("", "I swer on me[] mum")),
        (c"[I swer on me{} mum]", Set("I swer on me{} mum")),
        (c"I swer on me{$str3} mum", Set("I swer on me"+str3+" mum", "I swer on me mum")),
        (c"I swer on me[$str3] mum", Set("I swer on me"+str3+" mum")),
        (c"[$rand]", Set(rand)),
        (c"{$rand}", Set(rand, "")),
        (c"$cleanRand", Set(cleanRand)),
        (c"[$cleanRand]", Set(cleanRand)),
        (c"{$cleanRand}", Set(cleanRand, "")),
        (c"$cleanRand[$cleanRand]$cleanRand{$cleanRand}", Set(cleanRand*3, cleanRand*4)),
        (c"$cleanRand[$rand]$cleanRand$rand", Set(cleanRand+rand+cleanRand+rand)),
        (c"$rand {:)|!|^_^}", Set(rand+" :)", rand+"!", rand+" ^_^"))
      )
    }

    for (i <- 1 to 5000; (cap, resSet) <- cases(); if (!resSet.contains(cap))) {
      println
      println(cap)
      resSet.foreach(println)
    }
  }
}
