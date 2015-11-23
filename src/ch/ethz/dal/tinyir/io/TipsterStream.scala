package ch.ethz.dal.tinyir.io

import ch.ethz.dal.tinyir.processing.{TipsterParse, XMLDocument}
import rx.lang.scala.Observable

class TipsterStream (path: String, ext: String = "") 
extends ParsedXMLStream(new ZipDirStream(path, "")){
  def stream : Observable[XMLDocument] = unparsed.stream.map(new TipsterParse(_))
  def length = unparsed.length
}

object TipsterStream  {

  def main(args: Array[String]) {
    val tipster = new TipsterStream ("/Users/thofmann/Data/Tipster/zips")  
    println("Number of files in zips = " + tipster.length)
    
    var length : Long = 0 
    var tokens : Long = 0
    for (doc <- tipster.stream.take(10000)) { 
      length += doc.content.length          
      tokens += doc.tokens.length
    }
    println("Final number of characters = " + length)
    println("Final number of tokens     = " + tokens)
  }
}
