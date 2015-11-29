package ch.ethz.ir.project2

import java.io.{FileOutputStream, ObjectOutputStream}

import ch.ethz.dal.tinyir.processing.TipsterCorpusIterator

object Exporter {

  def main(args: Array[String]) {
    val oos = new ObjectOutputStream(new FileOutputStream("alldocs_.dat"))
    new ProgressIndicatorWrapper(
      new TipsterCorpusIterator("/Users/david/Downloads/IR2015/tipster/zips"), 1078000)//.foreach(oos.writeUnshared)
              .zipWithIndex
          .foreach { case (doc, i) =>
              oos.writeUnshared(TipsterDocument(doc))
              if (i % 1000 == 0) {
                oos.reset()
              }
            }
    oos.close()
  }
}
