package ch.ethz.ir.project2


object AveragePrecision {
  def evaluate[A](retriev: List[A], relev: Set[A]): Double = {
    var sum = 0.0
    var size = 0
    var correct = 0.0
    for (elem <- retriev) {
      size += 1
      if (relev contains elem) {
        correct += 1
        sum += correct / size
      }

    }
    if (correct == 0) {
      0
    }
    else {
      sum / correct
    }
  }

  def main(args: Array[String]) {
    var retrieved = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    var relevant = Set(1, 3, 6, 9, 10)
    assert(Math.abs(AveragePrecision.evaluate(retrieved, relevant) - 0.62) < 0.01)
  }

}
