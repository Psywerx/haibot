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
  def stringFilter(str: String): String = {
    str
      .toLowerCase
      .replaceAll("[,.!?-_:;]", " ")
      .replaceAll("[^a-zA-Z\\s]", "")
      .replaceAll("\\s+", " ").trim.split(" ").map(_.trim)
      .filter(word => (word.size >= 2 || allowedSingleLetters.contains(word)))
      .mkString(" ")
  }

  def fromFile(name: String): String = {
    val file = io.Source.fromFile(name)
    val out = file.mkString
    file.close
    out
  }

  def OCR(path: String): Option[String] = {
    // TODO: put params in file, us mktemp for temp files, pay some more attention to security
    val engineCnt = 5
    val dst = "/tmp/ocr"
    val tmpFile = path.replaceAll("[^a-zA-Z]", "").take(7)+nextInt(1000)+("."+path.reverse.takeWhile(_ != '.').replaceAll("[^a-zA-Z.]", "").take(7).reverse).replaceAll("[.]+", ".")

    try { (s"mkdir -p $dst").!! } catch { case e: Exception => return None }
    
    val results = (0 until engineCnt) flatMap { engine =>
      try {
        (s"""rm $dst/$tmpFile.txt""").!
        (s"""rm $dst/$tmpFile""").!
        (s"""rm $dst/$tmpFile.pnm""").!

        Await.result(future {
          // preprocess
          val params = "-resize 640x640> " + (engine match {
            //general
            case 0 => "-negate, -sigmoidal-contrast 14x16%, -threshold 25%, -background gray0, -deskew 8%, -sigmoidal-contrast 8x84%, -threshold 40%, -contrast-stretch 0x65%"
            case 1 => "-shear 1.0x1.0, -deskew 60%, -negate, -morphology Convolve Diamond:1, -swirl 0.2, -auto-gamma, -threshold 58%, -scale 100%x97%, -colorspace Gray, -sigmoidal-contrast 12x80%"
            case 2 => "-virtual-pixel Dither, +distort SRT -0.3, +contrast, -white-threshold 90%, -threshold 63%, -colorspace Gray, -gravity center, -extent 110%x104%, -negate"
            case 3 => "-scale 107%, -negate, -scale 112%x100%, -liquid-rescale 99%x101%, -sharpen 3x6, -contrast-stretch 0x38%, -threshold 12%, -deskew 70%"
            //memes
            case 4 => "-negate -sigmoidal-contrast 14x12% -scale 115%x102% -threshold 24% -background gray0 -deskew 9% -threshold 47%"
            case 5 => "-shear 3.12x2.79 -deskew 62% -negate -morphology Convolve Diamond:1 -swirl -0.3 -auto-gamma -white-threshold 55% -scale 100%x94% -colorspace Gray -sigmoidal-contrast 12x80%"
          }).replaceAll(", "," ")
          
          (s"""convert $path $params $dst/$tmpFile"""+(if(engine == 3) ".pnm" else "")).!
          
          // OCR
          engine match {
            case 0     => (s"""tesseract $dst/$tmpFile $dst/$tmpFile""").!!
            case 1     => (s"""gocr -C a-zA-Z -i $dst/$tmpFile -o $dst/$tmpFile.txt""").!!
            case 2|4|5 => (s"""cuneiform $dst/$tmpFile -o $dst/$tmpFile.txt""").!!
            case 3     => (s"""ocrad -lf --filter=letters --format=utf8 -o $dst/$tmpFile.txt $dst/$tmpFile.pnm""").!!
          }
        }, 20.seconds)
        
        val out = Some(stringFilter(fromFile(s"$dst/$tmpFile.txt")))

        (s"""rm $dst/$tmpFile.txt""").!
        (s"""rm $dst/$tmpFile""").!
        (s"""rm $dst/$tmpFile.pnm""").!

        out
      } catch {
        case e: Exception => 
          println(s"EXCEPTION in $path ... $e")
          None
      }
    }
    
    def commonWords(strings: Seq[String]): Set[String] = {
      strings
        .map(_.split(" ").distinct)
        .reduce(_ ++ _)
        .groupBy(a => a)
        .filter(_._2.size >= 2).keys // at least 2 occurences of word
        .filter(_.size >= 2).toSet   // word at least length 2
    }
        
    def selectResult(results: Seq[String]): Option[String] = {
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
      
      Some((attrs sortWith { case ((_,a),(_,b)) => 
        if(a(COMMON) == b(COMMON)) {
          (a(AVGLEN) > b(AVGLEN))
        } else {
          (a(COMMON) > b(COMMON)) 
        }
      } head)._1)
    }

    println(results.mkString("\n"))
    selectResult(results)
  }
}

