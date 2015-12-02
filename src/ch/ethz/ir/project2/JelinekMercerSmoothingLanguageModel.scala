package ch.ethz.ir.project2

import ch.ethz.dal.tinyir.lectures.TermFrequencies
import ch.ethz.ir.project2.RetrievalSystem.Query

class JelinekMercerSmoothingLanguageModel(lambda: Double) extends Model {

  implicit val termExtractor = TermExtractor(shouldSplit = true, shouldStem = true)

  def score(input: ModelInput): Unit = {
    val tipsterCorpusIterator = new TipsterUnzippedIterator(FilePathConfig.unzippedCorpus)
    val cfSum = input.collectionFrequency.values.sum.toDouble

    val queries = input.topics.map(Query(_, 1)(termExtractor))
    var progressInt = 0
    for (doc <- tipsterCorpusIterator) {
      val docTerms = doc.terms(1)
      val docTermsSet = docTerms.toSet
      val tf = TermFrequencies.tf(docTerms)
      val tfSum = tf.values.sum.toDouble
      if (tfSum > 0) {
        for (query <- queries) {
          var sumlogPwd = math.log(lambda)
          for (term <- docTermsSet.intersect(query.termsSet)) {
            val pHatwd = tf.getOrElse(term, 0) / tfSum
            val pw = input.collectionFrequency.getOrElse(term, 0) / cfSum
            sumlogPwd += math.log(1 + (1 - lambda / lambda * pHatwd / pw))
          }
          add(query.id, ScoredResult(doc.name, sumlogPwd))
        }
      }
      progressInt += 1
      progress = progressInt / tipsterCorpusIterator.size.toDouble
    }
  }

  val name = s"jm-$lambda"
}
