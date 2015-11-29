package ch.ethz.ir.project2

class ProgressIndicatorWrapper[T](wrapped: Iterator[T], size: Int) extends Iterator[T] {

  var progress = 0
  val started = System.currentTimeMillis()

  protected def set(prog: Int): Unit = {
    progress = prog
    if (prog == 1 || (prog - 1) * 1000 / size < prog * 1000 / size) {
      print("\r" + prog * 1000 / size / 10.0 + "% " + remainingFormatted)
    }
  }

  def remainingSecs = {
    if (progress > 0) {
      ((size - progress) / (progress / (System.currentTimeMillis() - started).toDouble) / 1000).toLong
    } else {
      0L
    }
  }

  private def remainingFormatted = {
    val s = remainingSecs
    if (s > 0) {
      " - %d:%02d remaining".format(s / 60, s % 60)
    } else {
      ""
    }
  }

  protected def increment(): Unit = {
    set(progress + 1)
  }

  def hasNext = wrapped.hasNext

  def next() = {
    increment()
    wrapped.next()
  }
}
