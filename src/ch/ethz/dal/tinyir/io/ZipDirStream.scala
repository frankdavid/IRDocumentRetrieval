package ch.ethz.dal.tinyir.io

import java.io.{File, InputStream}

import rx.lang.scala.Observable
import rx.lang.scala.schedulers.IOScheduler

// create a document stream out of all files in a all zip files
// that are found in a given directory
//
class ZipDirStream (dirpath: String, extension: String = "") 
extends DirStream (dirpath,extension) {

  override def length : Int =
    ziplist.map(new ZipStream(_,extension).length).sum
  
/*  override def stream : Stream[InputStream] = {  
    ziplist.map(new ZipStream(_,extension).stream).reduceLeft(_ append _)
  }
*/
  override def stream : Observable[InputStream] =
    Observable.from(ziplist).flatMap(new ZipStream(_,extension).stream.subscribeOn(IOScheduler()))
  
  val ziplist = new File(dirpath)
      .listFiles.filter(isZipFile)
  	  .map(z => z.getAbsolutePath).sorted.toList
  
  private def isZipFile(f: File) = f.getName.endsWith(".zip")
}

object ZipDirStream {
  def main(args: Array[String]) {
    val path = "/Users/thofmann/Data/Tipster/zips"
    val docs = new ZipDirStream (path)
    println("Reading from path = " + path)
    println("Number of documents = " + docs.length)
  }
}
