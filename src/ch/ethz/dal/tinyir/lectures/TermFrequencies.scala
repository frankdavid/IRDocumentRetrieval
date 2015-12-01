package ch.ethz.dal.tinyir.lectures

import java.lang.Math.log10

import scala.collection.mutable
import scala.collection.Map

object TermFrequencies {

  def tf(doc : Seq[String]) : Map[String, Int] = {
    val map = new mutable.HashMap[String, Int]
    doc.foreach { w =>
      map(w) = map.getOrElse(w, 0) + 1
    }
    map
  }
  
  def logtf(doc : Seq[String]) : Map[String, Double] =
    logtf(tf(doc))
    
  def atf(doc : List[String]) : Map[String, Double] = { 
    val atf = tf(doc)
    atf.mapValues(f => 0.5 * f /atf.values.max.toDouble + 0.5)
  }
    
  def logtf(tf: Map[String,Int]) : Map[String, Double] = {
    tf.mapValues(v => math.log(v.toDouble + 1))
  }
  
  def idf(df: Map[String,Int], n: Int) : Map[String, Double] = {
    val logn = math.log(n)
    df.mapValues(logn - math.log(_))
  }

  def log2 (x : Double) = log10(x)/log10(2.0)

  def tfIdf(query: Seq[String], ltf: Map[String, Double], idf: Map[String, Double]): Double = {
    var sum = 0d
    for (w <- query) {
      sum += ltf.getOrElse(w, 0d) * idf.getOrElse(w, 0d)
    }
    sum
  }

  def main(args: Array[String]) = {
    val query : Set[String] = Set("green", "blue", "powder") 
    val doc : List[String] = List("green", "blue", "red", "green")
  
    
    
    val score = query.flatMap(logtf(doc) get).sum 
    println(score)
  }
}
