package ch.ethz.dal.tinyir.processing

class StopWords {
  val stopWords = scala.io.Source.fromFile("resources/stopwords.txt").mkString.split("\n").map(_.trim).toSet
  def filter(tokens : Seq[String]) = tokens.filter(!stopWords.contains(_))
}
