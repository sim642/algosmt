package eu.sim642.algosmt.logic.idl

import scala.collection.mutable

object BellmanFord {
  type Weight = Int
  type Edge[A] = (A, Weight, A)

  def bellmanFord[A](vertices: Set[A], edges: Set[Edge[A]], source: A): Either[Set[Edge[A]], Map[A, Weight]] = {
    // https://en.wikipedia.org/wiki/Bellman%E2%80%93Ford_algorithm#Algorithm

    // step 1: initialize graph
    val distance: mutable.Map[A, Weight] = mutable.Map.empty
    val predecessor: mutable.Map[A, Edge[A]] = mutable.Map.empty
    distance(source) = 0

    // step 2: relax edges repeatedly
    for {
      i <- 1 until vertices.size
      edge@(u, w, v) <- edges
      if distance.contains(u)
      if !distance.contains(v) || (distance(u) + w < distance(v))
    } {
      distance(v) = distance(u) + w
      predecessor(v) = edge
    }

    // step 3: check for negative-weight cycles
    for {
      edge@(u, w, v) <- edges
      if distance.contains(u)
      if !distance.contains(v) || (distance(u) + w < distance(v))
    } {
      val cycle: mutable.Buffer[A] = mutable.Buffer.empty
      var x = v
      do {
        cycle += x
        val (x2, _, _) = predecessor(x)
        x = x2
      } while (!cycle.contains(x))
      val xi = cycle.indexOf(x)
      val cycle2 = cycle.drop(xi)
      //println(cycle2)
      val cycleEdges = cycle2.map(predecessor(_))
      //println(cycleEdges)
      return Left(cycleEdges.toSet)
    }

    Right(distance.toMap)
  }

  def main(args: Array[String]): Unit = {
    println(bellmanFord[String](
      Set("x0", "x1", "x2", "x3"),
      Set(
        ("x0", 0, "x1"),
        ("x0", 0, "x2"),
        ("x0", 0, "x3"),

        ("x2", 2, "x1"),
        ("x3", 1, "x2"),
        ("x1", -1, "x3"),
      ),
      "x0"
    ))

    println(bellmanFord[String](
      Set("x0", "x1", "x2", "x3"),
      Set(
        ("x0", 0, "x1"),
        ("x0", 0, "x2"),
        ("x0", 0, "x3"),

        ("x2", 2, "x1"),
        ("x3", 1, "x2"),
        ("x1", -4, "x3"),
      ),
      "x0"
    ))
  }
}
