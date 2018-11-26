package eu.sim642.algosmt.idl

import scala.collection.mutable

object BellmanFord {
  type Vertex = String
  type Weight = Int
  type Edge = (Vertex, Weight, Vertex)

  def bellmanFord(vertices: Seq[Vertex], edges: Seq[Edge], source: Vertex): Option[Map[Vertex, Weight]] = {
    // https://en.wikipedia.org/wiki/Bellman%E2%80%93Ford_algorithm#Algorithm

    // step 1: initialize graph
    val distance: mutable.Map[Vertex, Weight] = mutable.Map.empty
    distance(source) = 0

    // step 2: relax edges repeatedly
    for {
      i <- 1 until vertices.size
      (u, w, v) <- edges
      if distance.contains(u)
      if !distance.contains(v) || (distance(u) + w < distance(v))
    } distance(v) = distance(u) + w

    // step 3: check for negative-weight cycles
    for {
      (u, w, v) <- edges
      if distance.contains(u)
      if !distance.contains(v) || (distance(u) + w < distance(v))
    } return None

    Some(distance.toMap)
  }

  def main(args: Array[String]): Unit = {
    println(bellmanFord(
      Seq("x0", "x1", "x2", "x3"),
      Seq(
        ("x0", 0, "x1"),
        ("x0", 0, "x2"),
        ("x0", 0, "x3"),

        ("x2", 2, "x1"),
        ("x3", 1, "x2"),
        ("x1", -1, "x3"),
      ),
      "x0"
    ))

    println(bellmanFord(
      Seq("x0", "x1", "x2", "x3"),
      Seq(
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
