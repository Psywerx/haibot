package org.psywerx

import scala.util.parsing.combinator._
import scala.util.Random._
import scala.language.postfixOps
import java.lang.StringBuilder

//TODO: oh god... sloppy implementation, make proper multilevel parsing :/
final object Caption extends RegexParsers {
  override val skipWhitespace = false
  
  def text = "[-_.,!?' a-zA-Z0-9()<>]+".r
  
  def group = """[\[][^\]]+[\]]([0-9](-[0-9])?)?""".r ^^ { g => 
    var str = g
    var repeats = 1
    str = 
      if(str.takeRight(3) matches "[1-9]-[0-9]") {
        val (n1,n2) = (str.takeRight(3).take(1).toInt, str.takeRight(1).toInt)
        repeats = nextInt(n2-n1 + 1) + n1
        str.dropRight(3)
      } else if(str.takeRight(1) matches "[1-9]") {
        repeats = nextInt(str.takeRight(1).toInt) + 1
        str.dropRight(1)
      } else {
        str
      }
    
    val out = str.drop(1).dropRight(1).split("[|]").toList
    (if(out.size > 1) out(nextInt(out.size)) else out.head) * repeats
  }
  def groupMaybe = """[{][^}]+[}]([0-9](-[0-9])?)?""".r ^^ { g => 
    var str = g
    var repeats = nextInt(2)
    str = 
      if(str.takeRight(3) matches "[0-9]-[0-9]") {
        val (n1,n2) = (str.takeRight(3).take(1).toInt, str.takeRight(1).toInt)
        repeats = nextInt(n2-n1 + 2) + n1
        if(repeats > n2) repeats = 0
        str.dropRight(3)
      } else if(str.takeRight(1) matches "[1-9]") {
        repeats = nextInt(str.takeRight(1).toInt + 1)
        str.dropRight(1)
      } else {
        str
      }

    val out = str.drop(1).dropRight(1).split("[|]").toList
    (if(out.size > 1) out(nextInt(out.size)) else out.head) * repeats
  }
  def quote = """[@][^@]*[@]""".r ^^ { g => g.drop(1).dropRight(1) }
  
  def apply(s: String): String = (
    parse((quote | ".".r)*, parse((group | groupMaybe | text | quote)*, s).get.mkString.replaceAll("\\s+", " ").trim).get.mkString.replaceAll("\\s+", " ").trim
            .replaceAll("%%%pipe%%%", "|")
            .replaceAll("%%%lbrack%%%", "[")
            .replaceAll("%%%rbrack%%%", "]")
            .replaceAll("%%%lbrace%%%", "{")
            .replaceAll("%%%rbrace%%%", "}")
  ).replaceAll(" ,", ",")
   .replaceAll(" [.]", ".")
   .replaceAll(" [!]", "!")
   .replaceAll(" [?]", "?")
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
            .replaceAll("[|]", "%%%pipe%%%")
            .replaceAll("[\\[]", "%%%lbrack%%%")
            .replaceAll("[\\]]", "%%%rbrack%%%")
            .replaceAll("[{]", "%%%lbrace%%%")
            .replaceAll("[}]", "%%%rbrace%%%")
            
        builder append ("@"+arg+"@")
        val part = pi.next()
        builder append process(part)
      }
      builder.toString
    }
  }
}
