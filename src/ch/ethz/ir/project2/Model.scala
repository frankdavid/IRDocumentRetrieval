package ch.ethz.ir.project2

import java.io.{File, PrintWriter}

import ch.ethz.dal.tinyir.lectures.PrecisionRecall

import scala.collection.mutable


abstract class Model {

  val queryHeaps = scala.collection.mutable.Map[Int, mutable.PriorityQueue[ScoredResult]]()

  abstract def score(input: ModelInput): Unit

  abstract def name: String

  abstract val termExtractor: TermExtractor

  var progress: Double = 0.0

  def run(input: ModelInput): PerformanceScore = {
    score(input)
    val performance = evaluate(input)
    writeTopicResults(input)
    println(s"Model $name finished")
    performance
  }

  def evaluate(input: ModelInput): PerformanceScore = {
    val topics = input.topics
    val benchmarks = input.benchmarks
    val report = new PrintWriter(new File(input.outputDir, s"$name-report"))
    val performancePerQuery = topics.sortBy(_.id).map { topic =>
      val predicted = queryHeaps(topic.id).map(_.title).toSet
      val actual = benchmarks(topic.id)
      val pr = PrecisionRecall.evaluate(predicted, actual)
      val f = FScore.evaluate(predicted, actual)
      val ap = AveragePrecision.evaluate(queryHeaps(topic.id).toList.sortWith(_.score > _.score).map(_.title), actual)
      topic -> PerformanceScore(pr.precision, pr.recall, f, ap)
    }
    performancePerQuery.foreach { case (topic, perf) =>
      report.println(s"${topic.id} ${topic.title}:")
      report.println(s"\tprec=${perf.precision} recall=${perf.precision} f-score=${perf.f1} ap=${perf.averagePrecision}")
    }
    val avgPerf = PerformanceScore(
      precision = performancePerQuery.map(_._2.precision).sum / topics.size,
      recall = performancePerQuery.map(_._2.recall).sum / topics.size,
      f1 = performancePerQuery.map(_._2.f1).sum / topics.size,
      averagePrecision = performancePerQuery.map(_._2.averagePrecision).sum / topics.size
    )
    report.println("Mean:")
    report.println(s"\tprec=${avgPerf.precision} recall=${avgPerf.precision} f-score=${avgPerf.f1} map=${avgPerf.averagePrecision}")
    avgPerf
  }

  def score(res: ScoredResult) = -res.score

  def add(id: Int, res: ScoredResult): Boolean = {
    val heap = queryHeaps.getOrElseUpdate(id, new mutable.PriorityQueue[ScoredResult]()(Ordering.by(score)))
    if (heap.size < 100) {
      // heap not full
      heap += res
      true
    } else if (heap.head.score < res.score) {
      heap.dequeue
      heap += res
      true
    } else false
  }

  def writeTopicResults(input: ModelInput): Unit = {
    val topicOutput = new PrintWriter(new File(input.outputDir, s"$name-topic"))
    val detailed = new PrintWriter(new File(input.outputDir, s"$name-topic-detailed"))
    for (topic <- input.topics) {
      val benchmark = input.benchmarks.getOrElse(topic.id, Set())
      val resultList = queryHeaps(topic.id).toList.sortBy(-_.score)
      for ((result, index) <- resultList.zipWithIndex) {
        topicOutput.println(topic.id + " " + (index + 1) + " " + result.title)
        detailed.println(topic.id + " " + (index + 1) + " " + result.title + "\t" + benchmark.contains(result.title))
      }
    }
    topicOutput.close()
    detailed.close()
  }

  case class ScoredResult(title: String, score: Double)
}
