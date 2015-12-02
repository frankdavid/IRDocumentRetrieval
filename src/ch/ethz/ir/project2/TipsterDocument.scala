package ch.ethz.ir.project2

import ch.ethz.dal.tinyir.processing.Document

@SerialVersionUID(100L)
class TipsterDocument(val body: String, val name: String) extends Document with Serializable {

  def title = ""

  def date = ""

  def content = body

  def terms(maxWindowSize: Int = 1)(implicit termExtractor: TermExtractor): Seq[String] = termExtractor.extractTokens(content, maxWindowSize)
}

object TipsterDocument {
  def apply(document: Document) = {
    new TipsterDocument(document.body, document.name)
  }
}
