package ch.ethz.ir.project2

import ch.ethz.dal.tinyir.processing.{Tokenizer, StopWords}

import scala.collection.mutable
import scala.io.Source

case class TermExtractor(shouldStem: Boolean, shouldSplit: Boolean, maxWindowSize: Int) {

  private val stopWords = new StopWords()

  private lazy val dictionary = {
    val source = Source.fromFile("dictionary.txt")
    val result = new mutable.HashSet[String]() ++= source.getLines()
    source.close()
    result
  }

  private def splitIntoWords(string: String): Seq[String] = {
    if (string.isEmpty) {
      Seq.empty
    } else if (dictionary contains string) {
      Seq(string)
    } else {
      val split = splitAtUpper(string)
      if (split.length > 1) {
        split
      } else {
        (1 until string.length).collectFirst {
          case splitPos if (dictionary contains string.substring(0, splitPos)) &&
              (dictionary contains string.substring(splitPos)) =>
            Seq(string.substring(0, splitPos), string.substring(splitPos))
        }.getOrElse(Seq(string)) // return original if cannot split
      }
    }
  }

  private def splitAtUpper(string: String): Seq[String] = {
    (1 until string.length).collectFirst {
      case splitPos if string(splitPos).isUpper => Seq(string.substring(0, splitPos), string.substring(splitPos))
    }.getOrElse(Seq(string))
  }

  def extractTokens(string: String): Seq[String] = {
    val tokens = Tokenizer.tokenize(string)
    extractTokens(tokens)
  }

  def extractTokens(tokens: Seq[String]): Seq[String] = {
    val split = if (shouldSplit) tokens.flatMap(splitIntoWords) else tokens
    val filtered = stopWords.filter(split)

    val stemmed = if (shouldStem) {
      filtered.map { word =>
        val stemmer = new Stemmer()
        stemmer.add(word)
        stemmer.stem()
      }
    } else {
      filtered
    }

    val lowered = stemmed.map(_.filter(_.isLetter).toLowerCase)
    if (maxWindowSize > 1) {
      (for (windowSize <- 1 to maxWindowSize; window <- lowered.sliding(maxWindowSize)) yield window.mkString(" ")).toSeq
    } else {
      lowered
    }
  }
}
