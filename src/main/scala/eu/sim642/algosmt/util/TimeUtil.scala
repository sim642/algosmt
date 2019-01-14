package eu.sim642.algosmt.util

object TimeUtil {
  def measureTime[A](f: => A): (A, Double) = {
    val startTime = System.nanoTime()
    val ret = f
    val endTime = System.nanoTime()
    val duration = endTime - startTime
    (ret, duration / 1000000000.0)
  }
}
