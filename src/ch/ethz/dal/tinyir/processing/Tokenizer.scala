package ch.ethz.dal.tinyir.processing

object Tokenizer {
  def tokenize (text: String) : Seq[String] =
    text.split("[ .,;:?!\\-\t\n\r\f]+")
}
