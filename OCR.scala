package org.psywerx

import sys.process._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.annotation.switch
import java.io.File
import java.nio.file.Files

object OCR {
  val allowedSingleLetters = Set("i", "a", "e", "o", "y", "u")
  def stringFilter(str: String): String = (
    str
      .toLowerCase
      .replaceAll("[,.!?:;_-]", " ")
      .replaceAll("[^a-zA-Z\\s]", "")
      .replaceAll("\\s+", " ").trim.split(" ").map(_.trim)
      .filter(word => (word.size >= 2 || allowedSingleLetters.contains(word)))
      .mkString(" ")
  )

  def commonWords(strings: Seq[String]): Set[String] = {
    strings
      .map(_.split(" ").distinct)
      .reduce(_ ++ _)
      .groupBy(a => a)
      .filter(_._2.size >= 2).keys // at least 2 occurrences of word
      .filter(_.size >= 2).toSet   // word at least length 2
  }

  def selectResult(results: Seq[String]): Option[String] =
    (results.size: @switch) match {
      case 0 => None
      case 1 => Some(results.head)
      case _ =>
        val common = commonWords(results)

        val attrs = results.map { result =>
          val resSplit = result.split(" ")
          (result, Array(
            result.size.toDouble,
            resSplit.count(word =>  common.contains(word)).toDouble,
            resSplit.count(word => !common.contains(word)).toDouble,
            resSplit.foldLeft(0d)((acc, res) => acc + res.size) / resSplit.size.toDouble
          ))
        }

        //TODO: put this in a case class or something :)

        val SIZE = 0
        val COMMON = 1
        val UNCOMMON = 2
        val AVGLEN = 3

        Some((attrs.sortWith { case ((_, a), (_, b)) =>
          if (a(COMMON) == b(COMMON)) {
            (a(AVGLEN) > b(AVGLEN))
          } else {
            (a(COMMON) > b(COMMON))
          }
        }.head)._1)
    }

  def OCR(file: String): Option[String] = OCR(new File(file))
  def OCR(file: java.io.File): Option[String] =
    if (!file.isFile) None
    else try {
      val fileStr = file.toString
      //TODO: put params in file
      val engineCnt = 3
      val results: Seq[String] =
        ((0 until engineCnt).flatMap { engine =>
          // Temp image and output text files
          val tempImg = Files.createTempFile("ocr_", ".pnm").toFile
          tempImg.deleteOnExit
          val tempImgName = tempImg.toString
          try {
            Await.result(future {
              // preprocess
              val convertParams = "-resize 640x640> " + ((engine: @switch) match {
                case 0 => "-negate -sigmoidal-contrast 14x16% -threshold 25% -background gray0 -deskew 8% -sigmoidal-contrast 8x84% -threshold 40% -contrast-stretch 0x65%"
                case 1 => "-shear 1.0x1.0 -deskew 60% -negate -morphology Convolve Diamond:1 -swirl 0.2 -auto-gamma -threshold 58% -scale 100%x97% -colorspace Gray -sigmoidal-contrast 12x80%"
                case 2 => "-scale 107% -negate -scale 112%x100% -liquid-rescale 99%x101% -sharpen 3x6 -contrast-stretch 0x38% -threshold 12% -deskew 70%"
              })

              //TODO: fails with spaces in names, quoting doesn't work, and it seems convertParams need to be passed as string
              val convertResult = (s"""convert $fileStr $convertParams $tempImgName""").!
              if (convertResult != 0) None
              else {
                // OCR
                val ocrText = stringFilter(((engine: @switch) match {
                  case 0 => Seq("tesseract", tempImgName, "stdout")
                  case 1 => Seq("gocr", "-C", "a-zA-Z", "-i", tempImgName)
                  case 2 => Seq("ocrad", "-lf", "--filter=letters", "--format=utf8", tempImgName)
                }).!!)

                if (ocrText.isEmpty) None
                else Some(ocrText)
              }
            }, 30.seconds)
          } catch {
            case e: Exception =>
              None
          } finally {
            tempImg.delete
          }
        })

      println(results.mkString("\n"))
      selectResult(results)
    } catch {
      case e: Exception =>
        println(s"EXCEPTION in $file ... $e")
        None
    }

}

