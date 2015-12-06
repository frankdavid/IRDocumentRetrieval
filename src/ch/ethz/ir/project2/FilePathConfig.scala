package ch.ethz.ir.project2

object FilePathConfig {

  val unzippedCorpus = "alldocs.dat"

  val topics = "/Users/david/Downloads/IR2015/tipster/topics"

  val zipPath = "/Users/david/Downloads/IR2015/tipster/zips"

  val qrels = "/Users/david/Downloads/IR2015/tipster/qrels"

  val frequencyPath = "data/"

  def documentFrequencyFileName(implicit termExtractor: TermExtractor): String = {
    frequencyPath + "/df" + termExtractorFileName + ".dat"
  }

  def collectionFrequencyFileName(implicit termExtractor: TermExtractor): String = {
    frequencyPath + "/cf" + termExtractorFileName + ".dat"
  }

  private def termExtractorFileName(implicit termExtractor: TermExtractor): String = {
    var name = ""
    if (termExtractor.shouldStem) {
      name += "_stemmed"
    }
    if (termExtractor.shouldSplit) {
      name += "_split"
    }
    name += termExtractor.maxWindowSize
    name
  }
}
