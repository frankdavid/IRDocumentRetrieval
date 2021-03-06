package ch.ethz.ir.project2

import ch.ethz.dal.tinyir.lectures.TermFrequencies
import ch.ethz.ir.project2.RetrievalSystem.Query

class TfIdfModel(val maxWindowSize: Int) extends Model {

  implicit val termExtractor = TermExtractor(true, true, maxWindowSize)

  def score(modelInput: ModelInput): Unit = {
    val topics = modelInput.topics
    val documentFrequency = modelInput.documentFrequency
    val tipsterCorpusIterator = new TipsterUnzippedIterator(FilePathConfig.unzippedCorpus)
    val queries = topics.map(Query(_)(termExtractor))
    val idf = TermFrequencies.idf(documentFrequency, tipsterCorpusIterator.size)

    var progressInt = 0
    for (doc <- tipsterCorpusIterator) {
      val logtf = TermFrequencies.logtf(doc.terms)
      for (query <- queries) {
        val tfIdf = TermFrequencies.tfIdf(query.terms, logtf, idf)
        add(query.id, ScoredResult(doc.name, tfIdf))
      }
      progressInt += 1
      progress = progressInt / tipsterCorpusIterator.size.toDouble
    }
  }

  val name = s"tfidf$maxWindowSize"
}
