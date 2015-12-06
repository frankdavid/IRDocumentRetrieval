package ch.ethz.ir.project2

import java.io._
import java.text.SimpleDateFormat
import java.util.Date

import scala.collection
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.matching.Regex

object RetrievalSystem {

  val benchmarks: Map[Int, Set[String]] = readBenchmarkData(FilePathConfig.qrels)

  val topics: IndexedSeq[Topic] = {
    val numberRegex = new Regex("Number: ([^<]+)")
    val titleRegex = new Regex("Topic: ([^<]+)")
    val conceptRegex = new Regex("(?s)Concept\\(s\\):([^<]+)")

    val topicsString = scala.io.Source.fromFile(FilePathConfig.topics).mkString

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


//  unfortunately this proved to be too slow
//
//  def scoreFactoredLanguageModel(): Unit = {
//    val tipsterCorpusIterator = new TipsterUnzippedIterator(FilePathConfig.unzippedCorpus)
//    val tagger = new MaxentTagger("resources/models/english-bidirectional-distsim.tagger")
//    for (doc <- new ProgressIndicatorWrapper(tipsterCorpusIterator)) {
//      val wordTagWordFrequency = mutable.Map[(String, String, String), Int]()
//      var score = 0d
//      val sentences = MaxentTagger.tokenizeText(new StringReader(doc.content))
//      for (sentence <- sentences) {
//        val tagged = tagger.tagSentence(sentence)
//        var prevWord: String = null
//        var prevTag: String = null
//        for (wordTag <- tagged) {
//          val tag = wordTag.tag()
//          val word = wordTag.word()
//          if (prevWord != null && prevTag != null) {
//            val key = (prevWord, prevTag, word)
//            wordTagWordFrequency(key) = wordTagWordFrequency.getOrElse(key, 0) + 1
//          }
//          prevWord = word
//          prevTag = tag
//        }
//      }
//    }
//  }

//  we could not finish this one :(
//
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

  def documentCollectionFrequency(implicit termExtractor: TermExtractor) = {
    val tipsterCorpusIterator = new TipsterUnzippedIterator(FilePathConfig.unzippedCorpus)
    val documentFrequencyFile = new File(FilePathConfig.documentFrequencyFileName)
    val collectionFrequencyFile = new File(FilePathConfig.collectionFrequencyFileName)
    if (documentFrequencyFile.exists() && collectionFrequencyFile.exists()) {
      println("Loading document & collection frequency tables from disk")
      val df =  {
        val dis = new ObjectInputStream(new FileInputStream(documentFrequencyFile))
        val documentFrequency = dis.readObject().asInstanceOf[scala.collection.Map[String, Int]]
        dis.close()
        documentFrequency
      }
      val cf = {
        val cis = new ObjectInputStream(new FileInputStream(collectionFrequencyFile))
        val collectionFrequency = cis.readObject().asInstanceOf[scala.collection.Map[String, Int]]
        cis.close()
        collectionFrequency
      }
      println("done")
      (df, cf)
    } else {
      println("Generating document & collection frequency table")
      val documentFrequency = mutable.Map[String, Int]()
      val collectionFrequency = mutable.Map[String, Int]()
      val corpus = new ProgressIndicatorWrapper[TipsterDocument](tipsterCorpusIterator)
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
      println("done")
      (documentFrequency, collectionFrequency)
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

  def main(args: Array[String]) {
    val models = Seq(
      new TfIdfModel(1),
      new TfIdfModel(2),
      new JelinekMercerSmoothingLanguageModel(0.3),
      new JelinekMercerSmoothingLanguageModel(0.5),
      new JelinekMercerSmoothingLanguageModel(0.8)
    )

    val date = new SimpleDateFormat("YYYYMMdd_HHmmss").format(new Date())
    val outputDir = new File(s"output/$date")
    outputDir.mkdirs()
    val frequencyFutures = models.map(_.termExtractor).toSet.map { termExtractor: TermExtractor =>
      Future {
        termExtractor -> documentCollectionFrequency(termExtractor)
      }
    }
    val frequenciesFuture = Future.sequence(frequencyFutures)
    val frequencies = Await.result(frequenciesFuture, Duration.Inf)
    val documentFrequencies = mutable.Map[TermExtractor, collection.Map[String, Int]]()
    val collectionFrequencies = mutable.Map[TermExtractor, collection.Map[String, Int]]()
    for ((termExtractor, (df, cf)) <- frequencies) {
      documentFrequencies(termExtractor) = df
      collectionFrequencies(termExtractor) = cf
    }
    val input = ModelInput(
      documentFrequencies = documentFrequencies,
      collectionFrequencies = collectionFrequencies,
      benchmarks = benchmarks,
      topics = topics,
      outputDir = outputDir
    )
    val resultFutures = models.map { model =>
      Future {
        model -> model.run(input)
      }
    }
    Future {
      while(true) {
        print("\r")
        models.foreach { model =>
          val progress = (model.progress * 1000).toInt / 10.0
          print(s"${model.name}: $progress%   ")
        }
        Thread.sleep(2000)
      }
    }
    val result = Await.result(Future.sequence(resultFutures), Duration.Inf)
    result.foreach(println)
  }


  case class Query(queryTopic: Topic)(implicit termExtractor: TermExtractor) {
    @inline def id: Int = queryTopic.id

    val terms: Seq[String] = {
      termExtractor.extractTokens(queryTopic.title)
    }

    val termsSet = terms.toSet
  }
}
