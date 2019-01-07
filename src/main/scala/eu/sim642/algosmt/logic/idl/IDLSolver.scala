package eu.sim642.algosmt.logic.idl

import eu.sim642.algosmt.logic.idl.BellmanFord.Edge
import eu.sim642.algosmt.smt.cnf.{NegLiteral, PosLiteral}
import eu.sim642.algosmt.smt.{LogicSolver, Model}

class IDLSolver[B] extends LogicSolver[Constraint[B], B, Int] {
  override def solve(model: Model[Constraint[B]]): Option[Map[B, Int]] = {
    val constraints: Set[Constraint[B]] = model.map({
      case PosLiteral(constraint) => constraint
      case NegLiteral(Constraint(x, y, n)) => Constraint(y, x, -n - 1) // !(x - y <= n) -> x - y > n -> y - x < -n -> y - x <= -n - 1
    })
    val variables = extractVariables(constraints)
    solve(variables, constraints)
  }

  def solve(variables: Set[B], constraints: Set[Constraint[B]]): Option[Map[B, Int]] = {
    val vertices: Set[B] = variables
    val edges: Set[Edge[B]] = constraints.map({ case Constraint(x, y, n) => (y, n, x) })
    val initialDistance: TraversableOnce[(B, Int)] = variables.view.map(_ -> 0)

    BellmanFord.bellmanFord(vertices, edges, initialDistance)
  }

  def extractVariables(constraints: Set[Constraint[B]]): Set[B] = {
    constraints.flatMap({ case Constraint(x, y, n) => Seq(x, y) })
  }
}
