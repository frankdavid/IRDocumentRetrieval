package ch.ethz.ir.project2

import ch.ethz.dal.tinyir.lectures.PrecisionRecall

/**
 * Created by Zalan on 11/18/2015.
 */
object FScore {

  def evaluate[A](retriev: Set[A], relev: Set[A]) : Double = {
    val pr = PrecisionRecall.evaluate(retriev, relev)
    var result = 0.0
    if (pr.precision != 0 && pr.recall != 0) {
      result = 2 * pr.precision * pr.recall / (pr.recall + pr.precision)
    }
    result

  }

}
