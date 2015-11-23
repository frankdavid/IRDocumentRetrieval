package ch.ethz.dal.tinyir.io
import java.io.InputStream
import java.io.BufferedInputStream
import java.io.FileInputStream

import rx.lang.scala.Observable

// Stream of InputStreams representing documents
//
abstract class DocStream {
  def stream : Observable[InputStream]
  def length : Int
}
object DocStream {
   def getStream(fname: String) = new BufferedInputStream(new FileInputStream(fname))
}
