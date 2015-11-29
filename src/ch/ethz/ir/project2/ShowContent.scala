package ch.ethz.ir.project2

import scala.io.StdIn

object ShowContent {

  def main(args: Array[String]) {
    val docName = StdIn.readLine()
    val result = new TipsterUnzippedIterator("alldocs.dat").collectFirst {
      case d if d.name == docName => d.content
    }
    println(result.getOrElse("No Result"))
  }

}
