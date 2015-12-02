package ch.ethz.ir.project2

import java.io._

import ch.ethz.dal.tinyir.lectures.{PrecisionRecall, TermFrequencies}
import edu.stanford.nlp.tagger.maxent.MaxentTagger

import scala.collection.mutable
import scala.util.matching.Regex
import scala.collection.JavaConversions._

object RetrievalSystem {

  implicit val termExtractor = TermExtractor(shouldStem = true, shouldSplit = true, maxWindowSize = 1)

  val queryHeaps = scala.collection.mutable.Map[Int, mutable.PriorityQueue[ScoredResult]]()

  val benchmarks = readBenchmarkData(FilePathConfig.qrels)

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
    val queries = topics.map(Query(_)(termExtractor))
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

  def scoreLanguageModelWithJelinekMercer(): Unit = {
    val tipsterCorpusIterator = new TipsterUnzippedIterator(FilePathConfig.unzippedCorpus)
    @inline def lambda(docTerms: Set[String]) = { 0.5
//      val t = docTerms.size / 500
//      0.8 - 0.8 * (1 / (1 + math.exp(-t)))
    }
    val cfSum = collectionFrequency.values.sum.toDouble
    println("Number of files in zips = " + tipsterCorpusIterator.size)


    val queries = topics.map(Query(_)(termExtractor))
    println(s"Calculating queries lambda = ${lambda(null)}")
    for (doc <- new ProgressIndicatorWrapper(tipsterCorpusIterator)) {
      val docTerms = doc.terms
      val docTermsSet = doc.terms.toSet
      val tf = TermFrequencies.tf(docTerms)
      val tfSum = tf.values.sum.toDouble
      if (tfSum > 0) {
        for (query <- queries) {
          var sumlogPwd = math.log(lambda(docTermsSet))
          for (term <- docTermsSet.intersect(query.termsSet)) {
            val pHatwd = tf.getOrElse(term, 0) / tfSum
            val pw = collectionFrequency.getOrElse(term, 0) / cfSum
            sumlogPwd += math.log(1 + (1 - lambda(docTermsSet)) / lambda(docTermsSet) * pHatwd / pw)
          }
          add(query.id, ScoredResult(doc.name, sumlogPwd))
        }
      }
    }
  }

  def scoreFactoredLanguageModel(): Unit = {
    val tipsterCorpusIterator = new TipsterUnzippedIterator(FilePathConfig.unzippedCorpus)
    val tagger = new MaxentTagger("resources/models/english-bidirectional-distsim.tagger")
    for (doc <- new ProgressIndicatorWrapper(tipsterCorpusIterator)) {
      val wordTagWordFrequency = mutable.Map[(String, String, String), Int]()
      var score = 0d
      val sentences = MaxentTagger.tokenizeText(new StringReader(doc.content))
      for (sentence <- sentences) {
        val tagged = tagger.tagSentence(sentence)
        var prevWord: String = null
        var prevTag: String = null
        for (wordTag <- tagged) {
          val tag = wordTag.tag()
          val word = wordTag.word()
          if (prevWord != null && prevTag != null) {
            val key = (prevWord, prevTag, word)
            wordTagWordFrequency(key) = wordTagWordFrequency.getOrElse(key, 0) + 1
          }
          prevWord = word
          prevTag = tag
        }
      }
//      for (query <- topics) {
//        query.title
//      }
    }
  }

//  def scoreDistanceModel(): Unit = {
//    val tipsterCorpusIterator = new TipsterUnzippedIterator(FilePathConfig.unzippedCorpus)
//    val extractor = termExtractor.copy(maxWindowSize = 1) // make sure, we are generating unigrams
//    val queries = topics.map(Query(_)(extractor))
//    for (doc <- new ProgressIndicatorWrapper(tipsterCorpusIterator)) {
//      val docTerms = doc.terms(extractor)
//      var i = 0
//      val termPositionMap = mutable.HashMap[String, List[Int]]
//      for (term <- docTerms) {
//        termPositionMap(term) = i :: termPositionMap.get(term, List.empty)
//        i += 1
//      }
//      for (query <- queries) {
//        var score = 0d
//        for (termPair <- query.terms.zipWithIndex.combinations(2)) {
//          val positions1 = termPositionMap(termPair(0)._1)
//          val positions2 = termPositionMap(termPair(1)._1)
//          val distanceInQuery = math.abs(termPair(0)._2 - termPair(1)._2)
//          var minDistance = Double.PositiveInfinity
//          for (position1 <- positions1; position2 <- positions2) {
//            minDistance = math.min(minDistance, math.abs(position1 - position2))
//          }
//          score += math.pow(math.max(0, minDistance - distanceInQuery), 2)
//        }
//      }
//    }
//  }

//  var  documentFrequency: Map[String, Int] = null
//  var collectionFrequency: Map[String, Int] = null
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

  def displayTopicResults(printWriter: PrintWriter): Unit = {
    for (topic <- topics) {
      val benchmark = benchmarks.getOrElse(topic.id, Set())
      val resultList = queryHeaps(topic.id).toList.sortBy(-_.score)
      for ((result, index) <- resultList.zipWithIndex) {
        printWriter.println(topic.id + " " + (index + 1) + " " + result.title)
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

  def evaluate(): Unit = {
    val performancePerQuery = topics.sortBy(_.id).map { topic =>
      val predicted = queryHeaps(topic.id).map(_.title).toSet
      val actual = benchmarks(topic.id)
      val pr = PrecisionRecall.evaluate(predicted, actual)
      val f = FScore.evaluate(predicted, actual)
      val ap = AveragePrecision.evaluate(queryHeaps(topic.id).toList.sortWith(_.score > _.score).map(_.title), actual)
      topic -> PerformanceScore(pr.precision, pr.recall, f, ap)
    }
    performancePerQuery.foreach { case (topic, perf) =>
      println(s"${topic.id} ${topic.title}:")
      println(s"\tprec=${perf.precision} recall=${perf.precision} f-score=${perf.f1} ap=${perf.averagePrecision}")
    }
    val avgPerf = PerformanceScore(
      precision = performancePerQuery.map(_._2.precision).sum / topics.size,
      recall = performancePerQuery.map(_._2.recall).sum / topics.size,
      f1 = performancePerQuery.map(_._2.f1).sum / topics.size,
      averagePrecision = performancePerQuery.map(_._2.averagePrecision).sum / topics.size
    )
    println("Mean:")
    println(s"\tprec=${avgPerf.precision} recall=${avgPerf.precision} f-score=${avgPerf.f1} map=${avgPerf.averagePrecision}")
  }

  def main(args: Array[String]) {
//    scoreTfIdf()
    scoreLanguageModelWithJelinekMercer()
//    scoreFactoredLanguageModel()
    val writer = new PrintWriter("ranking-l-5.run")
    displayTopicResults(writer)
    writer.close()
    evaluate()
  }

  case class ScoredResult(title: String, score: Double)

  case class Query(queryTopic: Topic)(implicit termExtractor: TermExtractor) {
    @inline def id: Int = queryTopic.id

    val terms: Seq[String] = {
      termExtractor.extractTokens(queryTopic.title) //++ termExtractor.extractTokens(queryTopic.concepts)
    }

    val termsSet = terms.toSet

//    lazy val
  }
}
