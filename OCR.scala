package org.psywerx

object OCR {
  import sys.process._
  import scala.util.Random._
  import collection.mutable.{Buffer,HashSet}
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.util._
  import scala.concurrent.duration._


  val allowedSingleLetters = Set("i", "a", "e", "o", "y", "u")
  def stringFilter(str:String): String = {
    str
      .toLowerCase
      .replaceAll("[,.!?-_:;]", " ")
      .replaceAll("[^a-zA-Z\\s]", "")
      .replaceAll("\\s+", " ").trim.split(" ").map(_.trim)
      .filter(word => (word.size >= 2 || allowedSingleLetters.contains(word)))
      .mkString(" ")
  }

  def fromFile(name:String) = {
    val file = io.Source.fromFile(name)
    val out = file.mkString
    file.close
    out
  }

  def OCR(path: String): Option[String] = {
    val engineCnt = 5
    val dst = "/tmp/ocr"
    val tmpFile = path.replaceAll("[^a-zA-Z]", "").take(7)+nextInt(1000)+("."+path.reverse.takeWhile(_ != '.').replaceAll("[^a-zA-Z.]", "").take(7).reverse).replaceAll("[.]+", ".")

    try { (s"mkdir -p $dst").!! } catch { case e:Exception => return None }
    
    val results = (0 until engineCnt).flatMap { engine =>
      try {
        (s"""rm $dst/$tmpFile.txt""").!

        Await.result(future {
          // preprocess
          val params = "-resize 640x640> " + (engine match {
            case 0 => "-deskew 79%, -brightness-contrast -1, -colorspace Gray, -white-threshold 96%"
            case 1 => "-auto-level, -sigmoidal-contrast 8x79%, -negate, -enhance, -fuzz 25%, -trim"
            case 2 => "-negate -sigmoidal-contrast 14x16% -threshold 25% -background gray0 -deskew 8% -sigmoidal-contrast 8x84% -threshold 40% -contrast-stretch 0x65%"
            case 3 => "-white-threshold 96% -negate -contrast -contrast-stretch 3x40% -brightness-contrast 50 -adaptive-blur 1x1 -contrast-stretch 2x27% -threshold 40% -brightness-contrast -80 -virtual-pixel Edge +distort SRT -0.2 -brightness-contrast +90 -unsharp 10x2 -auto-gamma -gamma 0.7 -gamma 1.25"
            case 4 => "-morphology Edge Diamond -blur 1x2 -unsharp 2x3 -sigmoidal-contrast 5x50% -fuzz 45% -floodfill 0x50% Black"
          }).replaceAll(", "," ")
      
          (s"""convert $path $params $dst/$tmpFile"""+(if(engine==3)".pnm"else"")).!
          
          // OCR
          engine match {
            case 0   => (s"""tesseract $dst/$tmpFile $dst/$tmpFile""").!!
            case 1   => (s"""gocr -C a-zA-Z -i $dst/$tmpFile -o $dst/$tmpFile.txt""").!!
            case 2|4 => (s"""cuneiform $dst/$tmpFile -o $dst/$tmpFile.txt""").!!
            case 3   => (s"""ocrad -lf --filter=letters --format=utf8 -o $dst/$tmpFile.txt $dst/$tmpFile.pnm""").!!              
          }
        }, 20.seconds)
        
        Some(stringFilter(fromFile(s"$dst/$tmpFile.txt")))
      } catch {
        case e: Exception => 
          println(s"EXCEPTION in $path ... $e")
          None
      } finally {
        (s"""rm $dst/$tmpFile.txt""").!
        (s"""rm $dst/$tmpFile""").!
        (s"""rm $dst/$tmpFile.pnm""").!
      }
    }
    
    def commonWords(strings: Seq[String]) = {
      strings
        .map(_.split(" ").distinct)
        .reduce(_ ++ _)
        .groupBy(a=> a)
        .filter(_._2.size >= 2).keys // at least 2 occurences of word
        .filter(_.size >= 2).toSet   // word at least length 2
    }
        
    def selectResult(results:Seq[String]): Option[String] = {
      if(results.size == 0) return None
      if(results.size == 1) return Some(results.head)
      
      val common = commonWords(results)
      
      val attrs = results.map { result => 
        val resSplit = result.split(" ")
        (result, Array(
          result.size, 
          resSplit.count(word =>  (common contains word)), 
          resSplit.count(word => !(common contains word)),
          resSplit.foldLeft(0d)((acc,res) => acc + res.size) / resSplit.size
        ))
      }
      
      val SIZE = 0
      val COMMON = 1 
      val UNCOMMON = 2
      val AVGLEN = 3
      
      Some(attrs.sortWith { case ((_,a),(_,b)) => 
        if(a(COMMON) == b(COMMON)) {
          (a(AVGLEN) > b(AVGLEN))
        } else {
          (a(COMMON) > b(COMMON)) 
        }
      }.head._1)
    }

    println(results.mkString("\n"))    
    selectResult(results)
  }
}

