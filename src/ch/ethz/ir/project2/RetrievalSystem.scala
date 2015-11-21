package ch.ethz.ir.project2

import ch.ethz.dal.tinyir.io.{TipsterStream, ZipDirStream, ReutersRCVStream}
import ch.ethz.dal.tinyir.lectures.{PrecisionRecall, TermFrequencies}
import ch.ethz.dal.tinyir.processing.{XMLDocument, Tokenizer, StopWords}
import com.github.aztek.porterstemmer.PorterStemmer

import scala.collection.mutable.PriorityQueue
import scala.util.matching.Regex

/**
 * Created by Zalan on 11/18/2015.
 */
object RetrievalSystem {

  var stopWords = new StopWords()
  var porterStemmer = new PorterStemmer()

  case class ScoredResult(title: String, score: Double)

  var queryHeaps = scala.collection.mutable.Map[Int, PriorityQueue[ScoredResult]]()

  def normalizeTokenList(tokens: List[String]) = {
    var stage1 = tokens.map(_.toLowerCase()).filter(!_.isEmpty)
    stopWords.filter(stage1).map(porterStemmer.stem(_))
  }

  def score(res: ScoredResult) = -res.score

  def add(id: Int, res: ScoredResult): Boolean = {
    queryHeaps(id) = queryHeaps.getOrElse(id, new PriorityQueue[ScoredResult]()(Ordering.by(score)))
    var heap = queryHeaps(id)
    if (heap.size < 100) {
      // heap not full
      heap += res
      true
    } else if (heap.head.score < res.score) {
      heap.dequeue
      heap += res
      true
    } else false
  }

  def score(path: String, topics: List[Topic]): Unit = {
    var tipster = new TipsterStream(path)
    println("Number of files in zips = " + tipster.length)

    var length: Long = 0
    var nrDocs: Int = 100000

    var df = scala.collection.mutable.Map[String, Int]()

    var allQueryWords = normalizeTokenList(Tokenizer.tokenize(topics.map(_.title).mkString(" "))).distinct.toSet
    //1st iteration to calculate document frequencies
    for (doc <- tipster.stream.take(nrDocs)) {
      var normalizedTokens = normalizeTokenList(doc.tokens).distinct.toSet
      normalizedTokens &= allQueryWords
      for (token <- normalizedTokens) {
        df(token) = 1 + df.getOrElse(token, 0)
      }

      //      println(normalizedTokens)
    }
    var idf = TermFrequencies.idf(df.toMap, nrDocs)

    //2nd iteration
    var i = 0
    for (doc <- tipster.stream.take(nrDocs)) {
      i += 1
      if (i % 1000 == 0) {
        println("At iteration: ", i)
      }
      var normalizedTokens = normalizeTokenList(doc.tokens)
      var termFreq = TermFrequencies.logtf(normalizedTokens)

      for (queryTopic <- topics) {
        var queryTokens = normalizeTokenList(Tokenizer.tokenize(queryTopic.title))
        add(queryTopic.id, ScoredResult(doc.name, TermFrequencies.tf_idf(queryTokens, termFreq, idf)))
      }
    }
  }

  case class Topic(title: String, id: Int)

  def getTopics(path: String): List[Topic] = {
    val patternNumber = new Regex("Number: .+")
    val patternTitle = new Regex("Topic: .+")

    var topics = scala.io.Source.fromFile(path).mkString

    var list = (patternNumber findAllIn topics).toList.map(_.replace("Number: ", "")) zip (patternTitle findAllIn topics).toList
    list.map(w => Topic(w._2.trim, w._1.trim.toInt))
  }

  def displayTopicResults(topics: List[Topic]): Unit = {
    for (topic <- topics) {
      var resultList = queryHeaps(topic.id).toList.sortWith(_.score > _.score).map(_.title).zipWithIndex.map(w => (w._1, w._2 + 1))
      for (result <- resultList) {
        println(topic.id + " " + result._2 + " " + result._1)
      }
    }
  }

  def readBenchmarkData(path: String): Map[Int, Set[String]] = {
    var lines = scala.io.Source.fromFile(path).mkString.split("\n")
    var resultMap = collection.mutable.Map[Int, Set[String]]()
    for (line <- lines) {
      if (line.endsWith("1")) {
        var topicId = line.split(" ")(0).toInt
        var fileId = line.split(" ")(2).replace("-", "")
        var newSet = resultMap.getOrElse(topicId, Set[String]())
        newSet += fileId
        resultMap(topicId) = newSet

      }
    }

    resultMap.toMap
  }

  def evaluate(benchmark: Map[Int, Set[String]], topicList: List[Topic]): Unit = {
    var map = 0.0
    for (topic <- topicList) {
      var predicted = queryHeaps(topic.id).map(_.title).toSet
      var actual = benchmark(topic.id).toSet & predicted
      var originalSize = benchmark(topic.id).size
      //fill actual with junk up to size 100
      while (actual.size < 100 && actual.size < originalSize) {
        actual = actual + actual.size.toString
      }
      var pr = PrecisionRecall.evaluate(predicted, actual)
      var f = FScore.evaluate(predicted, actual)
      var ap = AveragePrecision.evaluate(queryHeaps(topic.id).toList.sortWith(_.score > _.score).map(_.title), actual)
      map += ap
      println(pr + ", F-score :" + f)
    }
    println("Map: ", map / topicList.size)

  }

  def main(args: Array[String]) {
    var benchmark = readBenchmarkData("C:\\Users\\Zalan\\Downloads\\IR2015\\tipster\\qrels")
    var topics = getTopics("C:\\Users\\Zalan\\Downloads\\IR2015\\tipster\\topics")
    RetrievalSystem.score("C:\\Users\\Zalan\\Downloads\\IR2015\\tipster\\zips", topics)
    displayTopicResults(topics)
    evaluate(benchmark, topics)
  }

}
