package ch.ethz.ir.project2

import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.lectures.{PrecisionRecall, TermFrequencies}
import ch.ethz.dal.tinyir.processing.{StopWords, Tokenizer}
import com.github.aztek.porterstemmer.PorterStemmer

import scala.collection.mutable
import scala.util.matching.Regex

object RetrievalSystem {

  var stopWords = new StopWords()
  var porterStemmer = new PorterStemmer()

  case class ScoredResult(title: String, score: Double)

  var queryHeaps = scala.collection.mutable.Map[Int, mutable.PriorityQueue[ScoredResult]]()

  def normalizeTokenList(tokens: List[String]) = {
    val stage1 = tokens.map(_.toLowerCase()).filter(!_.isEmpty)
    stopWords.filter(stage1).map(porterStemmer.stem)
  }

  def score(res: ScoredResult) = -res.score

  def add(id: Int, res: ScoredResult): Boolean = {
    queryHeaps(id) = queryHeaps.getOrElse(id, new mutable.PriorityQueue[ScoredResult]()(Ordering.by(score)))
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
    val tipster = new TipsterStream(path)
    println("Number of files in zips = " + tipster.length)

    val nrDocs: Int = 10000

    val df = scala.collection.mutable.Map[String, Int]()

    //1st iteration to calculate document frequencies
    for (doc <- tipster.stream.take(nrDocs)) {
      val normalizedTokens = normalizeTokenList(doc.tokens).distinct
      for (token <- normalizedTokens) {
        df(token) = 1 + df.getOrElse(token, 0)
      }

      //      println(normalizedTokens)
    }

    val idf = TermFrequencies.idf(df.toMap, nrDocs)

    //2nd iteration
    var i = 0
    for (doc <- tipster.stream.take(nrDocs)) {
      i += 1
      if (i % 1000 == 0) {
        println("At iteration: ", i)
      }
      val normalizedTokens = normalizeTokenList(doc.tokens)
      val termFreq = TermFrequencies.logtf(normalizedTokens)

      for (queryTopic <- topics) {
        val queryTokens = normalizeTokenList(Tokenizer.tokenize(queryTopic.title))
        add(queryTopic.id, ScoredResult(doc.name, TermFrequencies.tf_idf(queryTokens, termFreq, idf)))
      }
    }
  }

  case class Topic(title: String, id: Int)

  def getTopics(path: String): List[Topic] = {
    val patternNumber = new Regex("Number: .+")
    val patternTitle = new Regex("Topic: .+")

    val topics = scala.io.Source.fromFile(path).mkString

    val list = (patternNumber findAllIn topics).toList.map(_.replace("Number: ", "")) zip (patternTitle findAllIn topics).toList
    list.map(w => Topic(w._2.trim, w._1.trim.toInt))
  }

  def displayTopicResults(topics: List[Topic]): Unit = {
    for (topic <- topics) {
      val resultList = queryHeaps(topic.id).toList.sortWith(_.score > _.score).map(_.title).zipWithIndex.map(w => (w._1, w._2 + 1))
      for (result <- resultList) {
        println(topic.id + " " + result._2 + " " + result._1)
      }
    }
  }

  def readBenchmarkData(path: String): Map[Int, Set[String]] = {
    val lines = scala.io.Source.fromFile(path).mkString.split("\n")
    val resultMap = collection.mutable.Map[Int, Set[String]]()
    for (line <- lines) {
      if (line.endsWith("1")) {
        val topicId = line.split(" ")(0).toInt
        var fileId = line.split(" ")(2).replace("-", "")
        var newSet = resultMap.getOrElse(topicId, Set[String]())
        newSet += fileId
        resultMap(topicId) = newSet

      }
    }

    resultMap.toMap
  }

  def evaluate(benchmark: Map[Int, Set[String]], topicList: List[Topic]): Unit = {
    for (topic <- topicList) {
      val pr = PrecisionRecall.evaluate(queryHeaps(topic.id).map(_.title).toSet, benchmark(topic.id))
      val f = FScore.evaluate(queryHeaps(topic.id).map(_.title).toSet, benchmark(topic.id))
      println(pr + ", F-score :" + f)
    }

  }

  def main(args: Array[String]) {
    val benchmark = readBenchmarkData("C:\\Users\\Zalan\\Downloads\\IR2015\\tipster\\qrels")
    val topics = getTopics("C:\\Users\\Zalan\\Downloads\\IR2015\\tipster\\topics")
    RetrievalSystem.score("C:\\Users\\Zalan\\Downloads\\IR2015\\tipster\\zips", topics)
    displayTopicResults(topics)
    evaluate(benchmark, topics)
  }

}
