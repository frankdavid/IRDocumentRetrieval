package ch.ethz.ir.project2

import ch.ethz.dal.tinyir.processing.Document

@SerialVersionUID(100L)
class TipsterDocument(val body: String, val name: String) extends Document with Serializable {

  def title = ""

  def date = ""

  def content = body

  def normalizedTokens: Seq[String] = Normalizer.normalizeTokenList(tokens)
}

object TipsterDocument {
  def apply(document: Document) = {
    new TipsterDocument(document.body, document.name)
  }
}
