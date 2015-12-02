package ch.ethz.ir.project2

import java.io._

import scala.collection.Map

case class ModelInput(documentFrequencies: Map[TermExtractor, Map[String, Int]],
                      collectionFrequencies: Map[TermExtractor, Map[String, Int]],
                      benchmarks: Map[Int, Set[String]],
                      topics: Seq[Topic],
                      outputDir: File
                     ) {
  def documentFrequency(implicit termExtractor: TermExtractor): Map[String, Int] = {
    documentFrequencies(termExtractor)
  }

  def collectionFrequency(implicit termExtractor: TermExtractor): Map[String, Int] = {
    collectionFrequencies(termExtractor)
  }
}
