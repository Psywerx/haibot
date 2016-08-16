package org.psywerx

import collection.mutable
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.annotation.switch
import scala.util.matching
import scala.util.matching._
import scala.util.Random._
import java.io._
import java.nio.file.Files

final object util {
  val folder = "util/"

  def withAlternative[T](func: => T, alternative: => T ): T = try { func } catch { case _: Throwable => alternative }
  def withExit[T](func: => T, exit: => Any = { }): T = try { func } catch { case _: Throwable => exit; sys.exit(-1) }
  def tryOption[T](func: => T): Option[T] = try { Some(func) } catch { case _: Throwable => None }

  def thread(func: => Unit): Unit =
    (new Thread(new Runnable {
      def run(): Unit =
        try { func } catch { case e: Exception => e.printStackTrace }
    })).start


  implicit class StringImplicits(val s: String) {
    def replace(m: (String, String)*): String =
      m.foldLeft(s)((out, rep) => out.replace(rep._1, rep._2))
    def replaceAll(m: (String, String)*): String =
      m.foldLeft(s)((out, rep) => out.replaceAll(rep._1, rep._2))
    //TODO: containsPercent: Double for fuzzy reasoning
    def containsAny(strs: String*): Boolean   = strs.exists(str => s.contains(str))
    def startsWithAny(strs: String*): Boolean = strs.exists(str => s.startsWith(str))
    def endsWithAny(strs: String*): Boolean   = strs.exists(str => s.endsWith(str))
    def sentences: Array[String] = s.split("[.!?]+") //TODO: http://stackoverflow.com/questions/2687012/split-string-into-sentences-based-on-periods
    private[this] val charMap = ("čćžšđ" zip "cczsd").toMap
    def makeEasy: String = // lazy way to make text processing easier
      s.toLowerCase
        .map(char => charMap.getOrElse(char, char))
        .replaceAll("[,:] ", " ")
    def maybe: String = if (nextBoolean) s else ""
    def findAll(r: Regex): List[String] = r.findAllIn(s).toList
    //def removeAll(rem: String): String = s.filterNot(rem contains _) //notsure wat
    def matches(r: Regex): Boolean = s.matches(r.toString)
    def distance(s2: String): Int = distance(s, s2)
    def distance(s1: String, s2: String): Int = {
      val memo = mutable.AnyRefMap[(List[Char], List[Char]), Int]()
      def min(a: Int, b: Int, c: Int): Int = math.min(math.min(a, b), c)
      def sd(s1: List[Char], s2: List[Char]): Int = {
        if (!memo.contains((s1, s2)))
          memo((s1, s2)) = (s1, s2) match {
            case (_, Nil) => s1.length
            case (Nil, _) => s2.length
            case (c1 :: t1, c2 :: t2) =>
              min(
                sd(t1, s2) + 1,
                sd(s1, t2) + 1,
                sd(t1, t2) + (if (c1 == c2) 0 else 1))
          }
        memo((s1, s2))
      }

      sd(s1.toList, s2.toList)
    }
  }

  //implicit class MaybeSI(val sc: StringContext) extends AnyVal { def maybe(args: Any*): String = sc.parts.iterator.mkString("").maybe }
  implicit class IntImplicits(val i: Int) extends AnyVal { def ~(j: Int): Int = nextInt(j-i+1)+i }

  implicit class Seqs[A](val s: Seq[A]) {
    def random: A = s(nextInt(s.size))
    def randomOption: Option[A] = if (s.isEmpty) None else Some(random)
  }
  implicit class Maps[A, B](val m: Map[A, B]) {
    def apply(a: A, b: B): B = m.getOrElse(a, b)
  }

  implicit class OptSeq[L <: Seq[_]](val optseq: Option[L]) { def emptyToNone: Option[L] = optseq filterNot { _.isEmpty } }

  implicit class D(val d: Double) { def prob: Boolean = nextDouble < d } //0.5.prob #syntaxabuse
  implicit class F(val f: Float) { def prob: Boolean = nextFloat < f }
  implicit class I(val i: Int) { def isBetween(min: Int, max: Int): Boolean = i >= min && i <= max }

  //TODO: force utf8 for reading and writing
  def getFile(name: String, allowFail: Boolean = false): List[String] = {
    try {
      val file = io.Source.fromFile(name)
      val out = file.getLines.toList
      file.close
      out
    } catch {
      case e: IOException if allowFail => Nil
    }
  }

  def withFile[E](fileName: String)(f: scala.io.BufferedSource => E): E = {
    val file = io.Source.fromFile(fileName)
    try {
      f(file)
    } finally {
      file.close
    }
  }

  //def using[A <: {def close(): Unit}, B](param: A)(func: A => B): B = try { func(param) } finally { param.close() }
  def using[A <: Closeable, B](param: A)(func: A => B): B = try { func(param) } finally { param.close() }

  def appendToFile(fileName: String, allowFail: Boolean = false)(textData: String): Unit =
    printToFile(fileName, allowFail)(textData, append = true)
  def printToFile(fileName: String, allowFail: Boolean = false)(textData: String, append: Boolean = false): Unit =
    try {
      using (new FileWriter(fileName, append)) {
        fileWriter => using (new PrintWriter(fileWriter)) {
          printWriter => printWriter.println(textData)
        }
      }
    } catch {
      case e: Exception if allowFail => ()
    }
}

object Regex {
  val anyURL = """(?i)\b((?:[a-z][\w-]+:(?:/{1,3}|[a-z0-9%])|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'".,<>?«»“”‘’]))""".r
  val webURL = """(?i)\b((?:https?:(?:/{1,3}|[a-z0-9%])|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'".,<>?«»“”‘’]))""".r
  val tweet = "https?://(?:www\\.)?twitter\\.com/.*/status(?:es)?/([0-9]+).*".r
}

object Memes {
  val NO_U = """http://bit.ly/yaJI5L"""
  val oh_you = """http://bit.ly/MvTwUG"""
  val so_fluffy = """http://bit.ly/MG5Hfx"""
  val it_was_you = """http://bit.ly/zg0jQt"""
  val thanks_alot = """http://i.imgur.com/NAE5F9i.png"""
}

object Time {
  //wish I knew a better way.
  import java.util.Date
  import java.text._
  import util._

  private val timeReg = """[\s,]{0,2}(ob\s)?((\d{1,2}:\d{2})|(\d{1,2}h))"""
  private val dateFormats = List[(matching.Regex, SimpleDateFormat)](
    (timeReg.r, new SimpleDateFormat("HH:mm")),

    (("""\d{1,2}[.]\d{1,2}""" + timeReg).r, new SimpleDateFormat("dd.MM HH:mm")),
    (("""\d{1,2}[.]\s[a-z]{3,}""" + timeReg).r, new SimpleDateFormat("dd. MMMM HH:mm")),
    (("""\d{1,2}\s[a-z]{3,}""" + timeReg).r, new SimpleDateFormat("dd MMMM HH:mm")),
    ("""\d{1,2}[.]\d{1,2}""".r, new SimpleDateFormat("dd.MM")),
    ("""\d{1,2}[.]\s[a-z]{3,}""".r, new SimpleDateFormat("dd. MMMM")),
    ("""\d{1,2}\s[a-z]{3,}""".r, new SimpleDateFormat("dd MMMM")),

    (("""\d{1,2}[.]\d{1,2}[.]\d{4}""" + timeReg).r, new SimpleDateFormat("dd.MM.yyyy HH:mm")),
    (("""\d{1,2}-\d{1,2}-\d{4}""" + timeReg).r, new SimpleDateFormat("dd-MM-yyyy HH:mm")),
    (("""\d{4}-\d{1,2}-\d{1,2}""" + timeReg).r, new SimpleDateFormat("yyyy-MM-dd HH:mm")),
    (("""\d{1,2}/\d{1,2}/\d{4}""" + timeReg).r, new SimpleDateFormat("MM/dd/yyyy HH:mm")),
    (("""\d{4}/\d{1,2}/\d{1,2}""" + timeReg).r, new SimpleDateFormat("yyyy/MM/dd HH:mm")),
    (("""\d{1,2}\s[a-z]{3}\s\d{4}""" + timeReg).r, new SimpleDateFormat("dd MMM yyyy HH:mm")),
    (("""\d{1,2}\s[a-z]{3,}\s\d{4}""" + timeReg).r, new SimpleDateFormat("dd MMMM yyyy HH:mm")),
    (("""\d{1,2}[.]\s[a-z]{3,}\s\d{4}""" + timeReg).r, new SimpleDateFormat("dd. MMMM yyyy HH:mm")),

    ("""\d{1,2}[.]\d{1,2}[.]\d{4}""".r, new SimpleDateFormat("dd.MM.yyyy")),
    ("""\d{1,2}-\d{1,2}-\d{4}""".r, new SimpleDateFormat("dd-MM-yyyy")),
    ("""\d{4}-\d{1,2}-\d{1,2}""".r, new SimpleDateFormat("yyyy-MM-dd")),
    ("""\d{1,2}/\d{1,2}/\d{4}""".r, new SimpleDateFormat("MM/dd/yyyy")),
    ("""\d{4}/\d{1,2}/\d{1,2}""".r, new SimpleDateFormat("yyyy/MM/dd")),
    ("""\d{1,2}\s[a-z]{3}\s\d{4}""".r, new SimpleDateFormat("dd MMM yyyy")),
    ("""\d{1,2}\s[a-z]{3,}\s\d{4}""".r, new SimpleDateFormat("dd MMMM yyyy")),
    ("""\d{1,2}[.]\s[a-z]{3,}\s\d{4}""".r, new SimpleDateFormat("dd. MMMM yyyy")))

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
      .replaceAll("[' ]o'clock$", ":00")
      .replaceAll("[,\\s]ob[\\s]", " ")
      .replaceAll("[,\\s]+", " ")

  def getDates(text: String, filter: (Date => Boolean) = (d: Date) => true): List[Date] = (
    if (text == null)
      Nil
    else
      dateFormats
        .flatMap { case (regex, format) =>
          text
            .findAll(regex)
            .flatMap(dateStr => tryOption(format.parse(deLocale(dateStr))))
            .map{ date => //TODO: hacky hack hack
              if (date.getMonth == 0 && date.getYear == 70) {
                date.setYear((new Date).getYear)
                date.setMonth((new Date).getMonth)
                date.setDate((new Date).getDate)
              } else if (date.getYear == 70) {
                date.setYear((new Date).getYear)
              }

              date
            }
            .filter(filter)
        }
        .distinct
        .sortWith((a, b) => b after a)
  )

  def getFutureDates(text: String): List[Date] = getDates(text, _.after(new Date))

  private val timeDivisor = 1000000L*1000L //s
  //val timeDivisor = 1000000L //ms
  def now: Int = (System.nanoTime/timeDivisor).toInt
  def since(time: Int): Int = now-time
  def time(func: => Unit): Int = {
    val startTime = now
    func
    now-startTime
  }
  private val sinceTimes = mutable.HashMap[Int, Int]()
  def since(timeRef: AnyRef): Int = {
    val hash = timeRef.hashCode
    val time = sinceTimes.getOrElseUpdate(hash, 0)
    val nowTime = now
    sinceTimes(hash) = nowTime

    nowTime-time
  }

  // used for uptime
  private def pad(i: Int, p: Int = 2): String = "0"*(p-i.toString.size)+i.toString
  def getSinceZeroString(time: Int): String = {
    val secs = time
    val (days, hours, minutes) = ((secs/60/60/24), pad((secs/60/60)%24), pad((secs/60)%60))

    (((days: @switch) match {
      case 0 => ""
      case 1 => s"$days day "
      case _ => s"$days days "
    }) + (if (secs < 90) s"$secs seconds" else if (minutes.toInt < 60 && hours.toInt == 0 && days == 0) s"${minutes.toInt} min" else s"$hours:$minutes"))
  }
  def getSinceString(time: Int): String = getSinceZeroString(since(time))
}

object Net {
  import de.l3s.boilerpipe.extractors._
  import java.net._
  import util._
  val extractor = KeepEverythingExtractor.INSTANCE

  //TODO: use download + file -i or some proper mime solution, this is risky
  def badExts: List[String] = getFile(util.folder+"badexts.db")
  def scrapeURLs(urls: String*): String =
    (urls
      .flatMap { _.findAll(Regex.webURL) }
      .map { _url =>
        val url = if (_url startsWith "www") "http://"+_url else _url //TODO: https everywhere :)
        try {
          if (url.endsWithAny(badExts: _*)) ""
          else extractor.getText(new URL(url))
        } catch {
          case e: Exception => // java.net.MalformedURLException
            e.printStackTrace()
            ""
        }
      }
      .mkString(" ")
      .trim)

  def tempDownload(url: String): Option[File] = {
    val tempFile = Files.createTempFile("tempdl_", null).toFile
    tempFile.deleteOnExit

    if (download(url, tempFile)) {
      Some(tempFile)
    } else {
      tempFile.delete
      None
    }
  }
  def withDownload[A](url: String)(func: Option[File] => A): A = {
    val tempFile = tempDownload(url)
    try {
      func(tempFile)
    } finally {
      tempFile.foreach { _.delete }
    }
  }
  def download(urlStr: String, outFile: File): Boolean = {
    try {
      import java.nio.channels._

      Await.ready(future {
        val url = new URL(urlStr)
        val rbc = Channels.newChannel(url.openStream())
        val fos = new FileOutputStream(outFile)
        try {
          fos.getChannel.transferFrom(rbc, 0, 1 << 24)
        } finally {
          fos.close
        }
      }, 10.seconds)

      true
    } catch {
      case e: Exception =>
        e.printStackTrace
        false
    }
  }
}

/// Store (still somewhat) based on bash :) #softwareanarchitecture
final object Store { def apply(file: String): Store = new Store(file) }
final class Store(file: String, keyFormat: String = """([-_a-zA-Z0-9]{1,16})""") {
  import sys.process._
  import util.{ getFile, appendToFile, printToFile }

  type Row = (String, String)
  private def rowToString(kv: Row): String = if (kv._2 != null) kv._1 + " " + kv._2 else kv._1

  def isValidKey(s: String): Boolean = s matches keyFormat
  def replaceWith(kvs: Seq[Row]): Unit = printToFile(file)(kvs.map(rowToString).mkString("\n"))
  def ++=(kvs: Iterable[Row]): Unit = appendToFile(file) { kvs.map(rowToString).mkString("\n") }
  def +=(k: String, v: String): Unit = appendToFile(file)(rowToString((k, v)))
  def +=(k: String): Unit = += (k, null)
  def +=(r: Row): Unit = appendToFile(file)(rowToString(r))
  def -=(k: String): Unit = Seq("sed", "-i", s"""/^$k$$\\|^$k[ ].*$$/d""", file).!! //TODO: dumps tmp files into folder sometimes
  def ?(k: String): List[String] = getFile(file, allowFail = true).filter(line => line.nonEmpty && (line == k || line.startsWith(k + " "))).map(_.drop(k.size + 1))
  def contains(k: String): Boolean = this.?(k).nonEmpty
  def containsAny(ks: Iterable[String]): Boolean = ks.exists(this.contains)
  def *(): List[Row] =
    getFile(file, allowFail = true) filterNot { _.isEmpty } map { res =>
      val sep = res.indexOf(' ')
      if (sep == -1) (res, null) else (res.substring(0, sep), res.substring(sep+1))
    }

  def toList = *
  def toMap = *.toMap

  def ?-(key: String): List[String] = {
    val out = ?(key)
    if (out.nonEmpty) this -= key
    out
  }
}
