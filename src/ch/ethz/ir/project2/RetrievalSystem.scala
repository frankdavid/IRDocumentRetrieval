package ch.ethz.ir.project2

import java.io.{ObjectInputStream, FileInputStream}

import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.lectures.{TermFrequencies, PrecisionRecall}
import ch.ethz.dal.tinyir.processing.Tokenizer

import scala.collection.mutable
import scala.util.matching.Regex

object RetrievalSystem {

  case class ScoredResult(title: String, score: Double)

  var queryHeaps = scala.collection.mutable.Map[Int, mutable.PriorityQueue[ScoredResult]]()

  def score(res: ScoredResult) = -res.score

  def add(id: Int, res: ScoredResult): Boolean = {
    val heap = queryHeaps.getOrElseUpdate(id, new mutable.PriorityQueue[ScoredResult]()(Ordering.by(score)))
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

  def score(path: String, topics: Seq[Topic]): Unit = {
//    println("Number of files in zips = " + tipster.length)
    val nrDocs = new TipsterStream("/Users/david/Downloads/IR2015/tipster/zips").length
    val tipster = new TipsterUnzippedIterator(path)//.take(nrDocs)
//    def stream = new TipsterStream(path).stream.tumbling(500).flatMap(_.map(Document).subscribeOn(ComputationScheduler()))
//    .tumbling(100).flatMap { buffer =>
//      buffer.map(Document).subscribeOn(ComputationScheduler())
//    }//.take(nrDocs)

//    println("Number of files in zips = " + tipster.length)


//    var allQueryWords = normalizeTokenList(Tokenizer.tokenize(topics.map(_.title).mkString(" "))).distinct.toSet
    //1st iteration to calculate document frequencies
//    val frequencies = stream.flatMapIterable(_.normalizedTokens)
//                            .foldLeft(Map[String, Int]()) { case (m, s) => m.updated(s, m.getOrElse(s, 0) + 1)}

//    val f = frequencies.toBlocking.first
//    println(f.maxBy(_._2))

//    val df = frequencies.map(_.mapValues(_.size))
//    df.foreach(println)

    val is = new ObjectInputStream(new FileInputStream("df.dat"))
    val df = is.readObject().asInstanceOf[scala.collection.immutable.Map[String, Int]]
    is.close()

//    val df = {
//       val df = mutable.Map[String, Int]()
//      new ProgressIndicatorWrapper(tipster.map(Document), nrDocs).foreach { doc =>
//        doc.normalizedTokens.foreach { token =>
//          df(token) = df.getOrElse(token, 0) + 1
//        }
//      }
//      val os = new ObjectOutputStream(new FileOutputStream("df.dat"))
//      os.writeObject(df.toMap)
//      os.close()
//      df.toMap
//    }


    //2nd iteration

    val queries = topics.map(Query)
    val idf = TermFrequencies.idf(df, nrDocs)
//    def top100(r: Seq[ScoredResult]) = r.sortBy(score).take(100)
//    val result = stream.flatMapIterable { doc =>
//      val logtf = TermFrequencies.logtf(doc.normalizedTokens)
//      queries.map { query =>
//        i += 1
////        if (i ) {
//          print("\r" + i)
////        }
//        query.id -> ScoredResult(doc.name, TermFrequencies.tfIdf(query.normalizedTokens, logtf, idf))
//      }
//    }.groupBy(_._1).map { case (id, scores) =>
//      id -> scores.map(_._2).slidingBuffer(1000, 1000).flatMapIterable(top100).toList.flatMapIterable(top100)
//    }
//    result.toBlocking.foreach { case (id, results) =>
//      results.toBlocking.foreach(println)
//    }


    for (doc <- new ProgressIndicatorWrapper(tipster, nrDocs)) {
      val logtf = TermFrequencies.logtf(doc.normalizedTokens)
      for (query <- queries) {
        val tfIdf = TermFrequencies.tfIdf(query.normalizedTokens, logtf, idf)
        add(query.id, ScoredResult(doc.name, tfIdf))
      }
    }
  }

  case class Query(queryTopic: Topic) {
    @inline def id: Int = queryTopic.id

    val normalizedTokens: Seq[String] = Normalizer.normalizeTokenList(Tokenizer.tokenize(queryTopic.title))
  }

  case class Topic(title: String, id: Int)

  def getTopics(path: String): Seq[Topic] = {
    val patternNumber = new Regex("Number: .+")
    val patternTitle = new Regex("Topic: .+")

    val topics = scala.io.Source.fromFile(path).mkString

    val list = (patternNumber findAllIn topics).map(_.replace("Number: ", "")) zip (patternTitle findAllIn topics).map(_.replace("Topic: ", ""))
    list.map(w => Topic(w._2.trim, w._1.trim.toInt)).toSeq
  }

  def displayTopicResults(topics: Seq[Topic]): Unit = {
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

  def evaluate(benchmark: Map[Int, Set[String]], topicList: Seq[Topic]): Unit = {
    var map = 0.0
    for (topic <- topicList) {
      val predicted = queryHeaps(topic.id).map(_.title).toSet
      var actual = benchmark(topic.id) & predicted
      val originalSize = benchmark(topic.id).size
      //fill actual with junk up to size 100
      while (actual.size < 100 && actual.size < originalSize) {
        actual = actual + actual.size.toString
      }
      val pr = PrecisionRecall.evaluate(predicted, actual)
      val f = FScore.evaluate(predicted, actual)
      var ap = AveragePrecision.evaluate(queryHeaps(topic.id).toList.sortWith(_.score > _.score).map(_.title), actual)
      map += ap
      println(pr + ", F-score :" + f)
    }
    println("Map: ", map / topicList.size)

  }

  def main(args: Array[String]) {
    val benchmark = readBenchmarkData("/Users/david/Downloads/IR2015/tipster/qrels")
    val topics = getTopics("/Users/david/Downloads/IR2015/tipster/topics")
    RetrievalSystem.score("alldocs.dat", topics)
//    RetrievalSystem.score("/Users/david/Downloads/IR2015/tipster/zips", topics)
    displayTopicResults(topics)
    evaluate(benchmark, topics)
  }

}
