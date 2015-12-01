package ch.ethz.ir.project2

import scala.io.StdIn

object ShowContent {

  def main(args: Array[String]) {
    implicit val termExtractor = TermExtractor(shouldStem = true, shouldSplit = true, maxWindowSize = 3)
    val docName = StdIn.readLine()
    val result = new TipsterUnzippedIterator("alldocs.dat").collectFirst {
      case d if d.name == docName => d
    }
    println(result.map(d => d.content + "\n").getOrElse("No Result"))
  }

}
