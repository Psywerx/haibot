package org.psywerx

import scala.collection.mutable
import org.psywerx.util._

class WordNet(folder: String) {
  //format is actually (id, word, type)... I should probably keep the type (noun, verb, ...)
  type wn_s = (Int, String)
  //These take a while at startup...
  val (wn_sById, wn_sByWord) = {
    val wnFile = io.Source.fromFile(folder+"wn_s2.db")
    val wn_sAll = wnFile.getLines.map(line => (line.take(9).toInt, line.drop(10).replace("''", "'"))).toList
    wnFile.close()
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
        if (allUpper) {
          1d
        } else if (allLower) {
          0d
        } else {
          val upper = word.count(char => char == Character.toUpperCase(char)).toDouble
          val lower = word.count(char => char == Character.toLowerCase(char)).toDouble
          upper/(upper+lower)
        }

      val synonyms =
        wn_sByWord(word.toLowerCase) // find word
          .map(_._1).distinct.flatMap(id => wn_sById(id).map(_._2)) // query all synonyms by id
          .filter(w => w != word && w.split(" ").forall(_.size >= 4)).distinct // filter probably useless ones

      if (synonyms.nonEmpty) {
        var outWord = synonyms.random

        if (outWord.toLowerCase == word.toLowerCase)
          outWord = word
        else if (allLower)
          outWord = outWord.toLowerCase
        else if (allUpper)
          outWord = outWord.toUpperCase
        else if (hasCapital && part <= 0.25)
          outWord = outWord.capitalize
        else if (outWord matches lowerCase) {
          outWord = outWord.map(char => if (part.prob) Character.toUpperCase(char) else Character.toLowerCase(char)) //see part
          if (hasCapital) outWord = outWord.capitalize
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

  def getMapFromwn(dbName: String): Map[Int, List[Int]] = {
    val wnFile = io.Source.fromFile(folder+dbName)
    val wnFileContent = wnFile.getLines.toList
    wnFile.close()

    wnFileContent
      .map(line => (line.take(9).toInt, line.drop(10).take(9).toInt))
      .groupBy(_._1).map(e => e._1 -> e._2.map(_._2)).withDefaultValue(List[Int]())
  }

  val wnDbs = List("hyp", "sim", "ins", "mm", "ms", "mp", "at").map(db => db -> getMapFromwn(s"wn_$db.db")).toMap
  val stoplist = {
    val file = io.Source.fromFile(folder+"stoplist")
    val out = file.getLines.toSet
    file.close()
    out
  }

  def getWeights: Map[String, Double] = {
    val file = io.Source.fromFile(folder+"weights")
    val out = file.getLines.toList.map { line =>
      val elts = line.split(" ")
      (elts(0), elts(1).toDouble)
    }.toMap.withDefaultValue(1d)
    file.close()
    out
  }

  def keywords(in: String, count: Int = 3): Option[List[String]] = {
    try {
      val scores = mutable.AnyRefMap[String, Double]()
      def addToScore(str: String, add: Double): Unit = {
        if (!stoplist.contains(str)) scores(str) = (scores.getOrElse(str, 0.0)+add)
      }

      val words = in.split(" ").toList flatMap {
        case wordReg(prefix, word, suffix) =>
          if (word.size >= 3) {
            preprocess(word) filter { w => wn_sByWord contains w } flatMap { w =>
              if (!stoplist.contains(w)) {
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

      for (db <- wnDbs; word <- words; wId <- db._2(word._1); relatedWord <- wn_sById(wId)) {
        addToScore(relatedWord._2, weights(db._1))
      }

      val out = (scores.toList.sortWith(_._2 > _._2).take(count+30).filterNot(e =>
        scores.exists(a =>
          (a._1 != e._1 && a._2 >= e._2 && a._1.size <= e._1.size) &&
          (a._1.startsWith(e._1.take(4)) || (e._1 contains a._1))
        )// take out similar words
      ).take(count).map(_._1)) //sort and convert to string

      Option(out).emptyToNone
    } catch {
      case e: Exception => e.printStackTrace; None
    }
  }
}

