package ch.ethz.ir.project2

import java.io.{FileOutputStream, ObjectOutputStream}

import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.processing.TipsterCorpusIterator

object Exporter {

  def main(args: Array[String]) {
    val length = new TipsterStream(FilePathConfig.zipPath).length
    val oos = new ObjectOutputStream(new FileOutputStream(FilePathConfig.unzippedCorpus))
    oos.writeInt(length)
    new ProgressIndicatorWrapper(
      new TipsterCorpusIterator(FilePathConfig.zipPath), length)
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
