package ch.ethz.ir.project2

import ch.ethz.dal.tinyir.lectures.PrecisionRecall
import ch.ethz.dal.tinyir.processing._
import com.github.aztek.porterstemmer.PorterStemmer

import scala.collection.immutable.{SortedBagConfiguration, TreeBag}
import scala.collection.mutable
import scala.util.matching.Regex

object RetrievalSystem {

  var stopWords = new StopWords()
  var porterStemmer = new PorterStemmer()

  case class ScoredResult(title: String, score: Double)

  var queryHeaps = scala.collection.mutable.Map[Int, mutable.PriorityQueue[ScoredResult]]()

  def normalizeTokenList(tokens: List[String]) = {
    val stage1 = tokens.map(_.filter(_.isLetter).toLowerCase).filter(!_.isEmpty)
    stopWords.filter(stage1).map { word =>
      val stemmer = new Stemmer()
      stemmer.add(word)
      stemmer.stem()
    }
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

  var k = 0
  def score(path: String, topics: Array[Topic]): Unit = {
//    val stream = tipster.stream.map(Document)
    /*
    val nrDocs: Int = 10000
    def stream = new TipsterStream(path).stream.sliding(200, 200).flatMap { buffer =>
      buffer.map(Document).subscribeOn(ComputationScheduler()).onBackpressureDrop
    }.take(nrDocs)

//    println("Number of files in zips = " + tipster.length)
    var length: Long = 0
    var nrDocs: Int = 100000

    var df = scala.collection.mutable.Map[String, Int]()

    var allQueryWords = normalizeTokenList(Tokenizer.tokenize(topics.map(_.title).mkString(" "))).distinct.toSet
    //1st iteration to calculate document frequencies
    val frequencies = stream.flatMapIterable(_.normalizedTokens).toMultiMap(identity)

    val f = frequencies.toBlocking.first
    println(f.maxBy(_._2.size))
    */
//    val df = frequencies.map(_.mapValues(_.size))
//    df.foreach(println)


//    val df = scala.collection.mutable.Map[String, Int]()
//    val stream = tipster.stream.take(10000).map(Document)
//    for (doc <- stream) {
//      val normalizedTokens = doc.normalizedTokens
//      for (token <- normalizedTokens) {
//        df(token) = 1 + df.getOrElse(token, 0)
//      }

      //      println(normalizedTokens)
//    }

/*
    val idf = TermFrequencies.idf(df.toMap, nrDocs)

    //2nd iteration
    var i = 0
    val queries = topics.map(Query)
    for (doc <- stream) {
      i += 1
      if (i % 1000 == 0) {
        println("At iteration: ", i)
      }
      val normalizedTokens = doc.normalizedTokens
      val termFreq = TermFrequencies.logtf(normalizedTokens)

      for (query <- queries) {
        add(query.id, ScoredResult(doc.name, TermFrequencies.tf_idf(query.normalizedTokens, termFreq, idf)))
      }
    }
  */
    Thread.sleep(2000000)
  }

  case class Query(queryTopic: Topic) {
    @inline def id: Int = queryTopic.id
    val normalizedTokens: Seq[String] = normalizeTokenList(Tokenizer.tokenize(queryTopic.title))
  }

  case class Document(val doc: XMLDocument) {
//    println(Thread.currentThread().hashCode())
    @inline def name = doc.name

    val normalizedTokens = normalizeTokenList(doc.tokens).distinct

    def normalizedTokensBag = TreeBag[String]()(SortedBagConfiguration.keepAll) ++ normalizedTokens
  }

  case class Topic(title: String, id: Int)

  def getTopics(path: String): Array[Topic] = {
    val patternNumber = new Regex("Number: .+")
    val patternTitle = new Regex("Topic: .+")

    val topics = scala.io.Source.fromFile(path).mkString

    val list = (patternNumber findAllIn topics).map(_.replace("Number: ", "")) zip (patternTitle findAllIn topics)
    list.map(w => Topic(w._2.trim, w._1.trim.toInt)).toArray
  }

  def displayTopicResults(topics: Array[Topic]): Unit = {
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
//    Thread.sleep(000)
    var i = 0
    new TipsterCorpusIterator("/Users/david/Downloads/IR2015/tipster/zips").map(Document).foreach { _ =>
      i += 1
      if (i % 1000 == 0) {
        println(i)
      }
    }
//    val benchmark = readBenchmarkData("/Users/david/Downloads/IR2015/tipster/qrels")
//    val topics = getTopics("/Users/david/Downloads/IR2015/tipster/topics")
//    RetrievalSystem.score("/Users/david/Downloads/IR2015/tipster/zips", topics)
//    displayTopicResults(topics)
//    evaluate(benchmark, topics)
  }

}
