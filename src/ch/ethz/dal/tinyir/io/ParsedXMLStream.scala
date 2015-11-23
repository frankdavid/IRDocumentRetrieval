package ch.ethz.dal.tinyir.io

import ch.ethz.dal.tinyir.processing.XMLDocument
import rx.lang.scala.Observable

abstract class ParsedXMLStream (val unparsed: DocStream) { 
  def stream : Observable[XMLDocument]
  def length : Int

}
