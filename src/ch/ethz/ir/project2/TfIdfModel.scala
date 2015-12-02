package ch.ethz.ir.project2

import ch.ethz.dal.tinyir.lectures.TermFrequencies
import ch.ethz.ir.project2.RetrievalSystem.Query

class TfIdfModel(val maxWindowSize: Int) extends Model {

  implicit val termExtractor = TermExtractor(true, true)

  def score(modelInput: ModelInput): Unit = {
    val topics = modelInput.topics
    val documentFrequency = modelInput.documentFrequency
    val tipsterCorpusIterator = new TipsterUnzippedIterator(FilePathConfig.unzippedCorpus)
    println("Number of files in zips = " + tipsterCorpusIterator.size)
    val queries = topics.map(Query(_)(termExtractor))
    val idf = TermFrequencies.idf(documentFrequency, tipsterCorpusIterator.size)

    println("Calculating queries")
    for (doc <- new ProgressIndicatorWrapper(tipsterCorpusIterator)) {
      val logtf = TermFrequencies.logtf(doc.terms(maxWindowSize))
      for (query <- queries) {
        val tfIdf = TermFrequencies.tfIdf(query.terms, logtf, idf)
        add(query.id, ScoredResult(doc.name, tfIdf))
      }
    }
  }

  abstract def name = s"tfidf$maxWindowSize"
}
