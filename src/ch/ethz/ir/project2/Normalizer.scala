package ch.ethz.ir.project2

import ch.ethz.dal.tinyir.processing.StopWords

import scala.io.Source
import scala.collection.mutable

object Normalizer {

  val stopWords = new StopWords()

  lazy val words = {
    val source = Source.fromFile("dictionary.txt")
    val result = new mutable.HashSet[String]() ++ source.getLines()
    source.close()
    result
  }

  def splitIntoWords(string: String): Seq[String] = {
    if (words contains string) {
      Seq(string, string)
    } else {
      (1 until string.length).collectFirst {
        case splitPos if (words contains string.substring(0, splitPos)) && (words contains string.substring(splitPos)) =>
          Seq(string, string.substring(0, splitPos), string.substring(splitPos))
      }.getOrElse(Seq(string, string)) // return original if cannot split
    }
  }

  def normalizeTokenList(tokens: Seq[String]): Seq[String] = {
    val stage1 = tokens.map(_.filter(_.isLetter).toLowerCase).filter(!_.isEmpty)
//        val sanitized = stage1
    val sanitized = stage1.flatMap(splitIntoWords)
    stopWords.filter(sanitized).map { word =>
      val stemmer = new Stemmer()
      stemmer.add(word)
      stemmer.stem()
    }
  }

}
