package org.psywerx

class Zemanta(apiKey: String) {
  import com.zemanta.api.{Zemanta => Zem, ZemantaResult}
  import com.zemanta.api.suggest.{Article, Keyword, Image}
  import scala.collection.JavaConversions.mapAsJavaMap
  import scala.collection.JavaConversions.asScalaBuffer
  
  def suggestKeywords(text: String, cnt: Int = 3): Option[List[String]] = suggest(text).map(_.getConfidenceSortedKeywords(true).toList.map(_.name).take(cnt))
  def suggestArticles(text: String): Option[List[Article]] = suggest(text).map(_.getConfidenceSortedArticles(true).toList)
  def suggestImages(text: String): Option[List[Image]] = suggest(text).map(_.getConfidenceSortedImages(true).toList)
  
  def suggest(text: String): Option[ZemantaResult] = {
    try {
      val zem = new Zem(apiKey, "http://api.zemanta.com/services/rest/0.0/")
      val request = new java.util.HashMap[String, String](Map(
        "method" -> "zemanta.suggest",
        "api_key" -> apiKey,
        "text" -> text,
        "format" -> "xml"
      ))

      val zemResult = zem.suggest(request)
      if(!zemResult.isError) {
        Some(zemResult)
      } else {
        None
      }
    } catch {
      case e: Exception => 
        e.printStackTrace
        None
    }
  }
}

