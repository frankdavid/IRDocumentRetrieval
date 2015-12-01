package ch.ethz.ir.project2

import java.io._

import ch.ethz.dal.tinyir.lectures.{PrecisionRecall, TermFrequencies}

import scala.collection.mutable
import scala.util.matching.Regex

object RetrievalSystem {

  implicit val termExtractor = TermExtractor(shouldStem = true, shouldSplit = true, maxWindowSize = 1)

  val queryHeaps = scala.collection.mutable.Map[Int, mutable.PriorityQueue[ScoredResult]]()

  val topics = {
    val numberRegex = new Regex("Number: ([^<]+)")
    val titleRegex = new Regex("Topic: ([^<]+)")
    val conceptRegex = new Regex("(?s)Concept\\(s\\):([^<]+)")

    val topicsString = scala.io.Source.fromFile(FilePathConfig.topics).mkString

//    val list = (numberRegex findAllIn topicsString).map(_.replace("Number: ", "")) zip (titleRegex findAllIn topicsString).map(_.replace("Topic: ", ""))

    val numbers = numberRegex.findAllMatchIn(topicsString).map(_.group(1))
    val titles = titleRegex.findAllMatchIn(topicsString).map(_.group(1))
    val concepts = conceptRegex.findAllMatchIn(topicsString).map { match_ =>
      val conceptString = match_.group(1)
      conceptString
          .split("(\\d+\\.|,)")
          .map(_.replaceAll("\\s+", " ").trim)
          .filterNot(c => c.startsWith("NOT") || c.isEmpty)
    }
    val topics = for (((number, title), concepts) <- (numbers zip titles) zip concepts) yield {
      Topic(title.trim, number.trim.toInt, concepts)
    }

    topics.toIndexedSeq
  }

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

  def scoreTfIdf(): Unit = {
    val tipsterCorpusIterator = new TipsterUnzippedIterator(FilePathConfig.unzippedCorpus)
    println("Number of files in zips = " + tipsterCorpusIterator.size)
    val queries = topics.map(Query)
    val idf = TermFrequencies.idf(documentFrequency, tipsterCorpusIterator.size)

    println("Calculating queries")
    for (doc <- new ProgressIndicatorWrapper(tipsterCorpusIterator)) {
      val logtf = TermFrequencies.logtf(doc.terms)
      for (query <- queries) {
        val tfIdf = TermFrequencies.tfIdf(query.terms, logtf, idf)
        add(query.id, ScoredResult(doc.name, tfIdf))
      }
    }
  }

  def scoreLanguageModel(): Unit = {
    val tipsterCorpusIterator = new TipsterUnzippedIterator(FilePathConfig.unzippedCorpus)
    val lambda = 0.0d
    val cfSum = collectionFrequency.values.sum.toDouble
    println("Number of files in zips = " + tipsterCorpusIterator.size)


    val queries = topics.map(Query)
    println("Calculating queries logtf lambda=" + lambda)
    for (doc <- new ProgressIndicatorWrapper(tipsterCorpusIterator)) {
      val tf = TermFrequencies.tf(doc.terms)
      val tfSum = tf.values.sum.toDouble
      if (tfSum > 0) {
        for (query <- queries) {
          var queryScore = 0d
          for (term <- query.terms) {
            val pHatwd = tf.getOrElse(term, 0) / tfSum
            val pwd = (1 - lambda) * pHatwd + lambda * (collectionFrequency.getOrElse(term, 0) / cfSum)
            queryScore += pwd
          }
          add(query.id, ScoredResult(doc.name, queryScore))
        }
      }
    }
  }

  val (documentFrequency, collectionFrequency) = {
    val tipsterCorpusIterator = new TipsterUnzippedIterator(FilePathConfig.unzippedCorpus)
    val documentFrequencyFile = new File(FilePathConfig.documentFrequencyFileName)
    val collectionFrequencyFile = new File(FilePathConfig.collectionFrequencyFileName)
    if (documentFrequencyFile.exists() && collectionFrequencyFile.exists()) {
      println("Loading document & collection frequency tables from disk")
      val dfFuture =/* Future */{
        val dis = new ObjectInputStream(new FileInputStream(documentFrequencyFile))
        val documentFrequency = dis.readObject().asInstanceOf[scala.collection.Map[String, Int]]
        dis.close()
        documentFrequency
      }
      val cfFuture = /*Future */{
        val cis = new ObjectInputStream(new FileInputStream(collectionFrequencyFile))
        val collectionFrequency = cis.readObject().asInstanceOf[scala.collection.Map[String, Int]]
        cis.close()
        collectionFrequency
      }
      val result = (/*Await.result(*/dfFuture/*, Duration.Inf)*/,
          /*Await.result(*/cfFuture/*, Duration.Inf)*/)
      println("Done")
      result
    } else {
      println("Generating document & collection frequency table")
      val documentFrequency = mutable.Map[String, Int]()
      val collectionFrequency = mutable.Map[String, Int]()
      val corpus = new ProgressIndicatorWrapper[TipsterDocument](tipsterCorpusIterator)
//      val frequencies = for {
//        group <- Observable.from(corpus).flatMapIterable(_.terms).groupBy(identity)
//        count <- group._2.subscribeOn(ComputationScheduler()).size
//      } yield (group._1, count)
//      val documentFrequency = frequencies.toBlocking.toIterable.toMap
      corpus.foreach { doc =>
        doc.terms.foreach { token =>
          collectionFrequency(token) = collectionFrequency.getOrElse(token, 0) + 1
        }
        doc.terms.distinct.foreach { token =>
          documentFrequency(token) = documentFrequency.getOrElse(token, 0) + 1
        }
      }
      val dos = new ObjectOutputStream(new FileOutputStream(documentFrequencyFile))
      dos.writeObject(documentFrequency)
      dos.close()
      val cos = new ObjectOutputStream(new FileOutputStream(collectionFrequencyFile))
      cos.writeObject(collectionFrequency)
      cos.close()
      (documentFrequency, collectionFrequency)
    }
  }

  def displayTopicResults(benchmarks: Map[Int, Set[String]]): Unit = {
    for (topic <- topics) {
      val benchmark = benchmarks.getOrElse(topic.id, Set())
      val resultList = queryHeaps(topic.id).toList.sortBy(-_.score)
      for ((result, index) <- resultList.zipWithIndex) {
        println(topic.id + " " + (index + 1) + " " + result.title + "\t" + benchmark.contains(result.title))
      }
    }
  }

  def readBenchmarkData(path: String): Map[Int, Set[String]] = {
    val lines = scala.io.Source.fromFile(path).mkString.split("\n")
    val resultMap = collection.mutable.Map[Int, Set[String]]()
    for (line <- lines) {
      if (line.endsWith("1")) {
        val parts = line.split(" ")
        val topicId = parts(0).toInt
        var fileId = parts(2).replace("-", "")
        var newSet = resultMap.getOrElse(topicId, Set[String]())
        newSet += fileId
        resultMap(topicId) = newSet

      }
    }

    resultMap.toMap
  }

  def evaluate(benchmark: Map[Int, Set[String]]): Unit = {
    var map = 0.0
    for (topic <- topics) {
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
    println("Map: ", map / topics.size)
  }

  def main(args: Array[String]) {
    val benchmark = readBenchmarkData(FilePathConfig.qrels)
//    scoreTfIdf()
    scoreLanguageModel()
    displayTopicResults(benchmark)
    evaluate(benchmark)
  }

  case class ScoredResult(title: String, score: Double)

  case class Query(queryTopic: Topic) {
    @inline def id: Int = queryTopic.id

    val terms: Seq[String] = {
      termExtractor.extractTokens(queryTopic.title) //++ termExtractor.extractTokens(queryTopic.concepts)
    }
  }
}
