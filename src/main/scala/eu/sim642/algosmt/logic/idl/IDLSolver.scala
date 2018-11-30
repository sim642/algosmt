package eu.sim642.algosmt.logic.idl

import eu.sim642.algosmt.logic.idl.BellmanFord.Edge
import eu.sim642.algosmt.smt.{LogicSolver, Model}
import eu.sim642.algosmt.{NegLiteral, PosLiteral}

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
    sealed trait ConstraintVertex
    case object SourceVertex extends ConstraintVertex
    case class VariableVertex(x: B) extends ConstraintVertex

    val vertices: Set[ConstraintVertex] = variables.map(VariableVertex).toSet[ConstraintVertex] + SourceVertex
    val edges: Set[Edge[ConstraintVertex]] =
      constraints.map({ case Constraint(x, y, n) => (VariableVertex(y), n, VariableVertex(x)) }) ++
        variables.map(x => (SourceVertex, 0, VariableVertex(x)))

    BellmanFord.bellmanFord(vertices, edges, SourceVertex).map({ distance =>
      distance.flatMap({
        case (SourceVertex, _) => None
        case (VariableVertex(x), value) => Some(x -> value)
      })
    })
  }

  def extractVariables(constraints: Set[Constraint[B]]): Set[B] = {
    constraints.flatMap({ case Constraint(x, y, n) => Seq(x, y) })
  }
}
