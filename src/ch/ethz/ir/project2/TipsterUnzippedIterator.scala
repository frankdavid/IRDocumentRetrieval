package ch.ethz.ir.project2

import java.io.{EOFException, FileInputStream, ObjectInputStream}

import scala.collection.AbstractIterator

class TipsterUnzippedIterator(fileName: String) extends AbstractIterator[TipsterDocument] {
  private val inputStream = new ObjectInputStream(new FileInputStream(fileName))
  private var _hasNext = true
  private var nextElem: TipsterDocument = _
  override val size = inputStream.readInt()

  readNext()

  def hasNext = _hasNext

  def next() = {
    if (_hasNext) {
      val ret = nextElem
      readNext()
      ret
    } else {
      Iterator.empty.next()
    }
  }

  private def readNext(): Unit = {
    try {
      nextElem = inputStream.readUnshared().asInstanceOf[TipsterDocument]
    } catch {
      case e: EOFException =>
        _hasNext = false
        inputStream.close()
    }
  }
}
