package eu.sim642.algosmt.logic.idl

import eu.sim642.algosmt.logic.idl.BellmanFord.Edge
import eu.sim642.algosmt.smt.cnf.{NegLiteral, PosLiteral}
import eu.sim642.algosmt.smt.{LogicSolver, Model}

class IDLSolver[B] extends LogicSolver[Constraint[B], B, Int] {
  override def solve(model: Model[Constraint[B]]): Either[Model[Constraint[B]], Map[B, Int]] = {
    val constraints: Set[Constraint[B]] = model.map({
      case PosLiteral(constraint) => constraint
      case NegLiteral(Constraint(x, y, n)) => Constraint(y, x, -n - 1) // !(x - y <= n) -> x - y > n -> y - x < -n -> y - x <= -n - 1
    })
    val variables = extractVariables(constraints)
    solve(variables, constraints) match {
      case Right(value) => Right(value)
      case Left(cycle) =>
        val allPosLiterals = cycle.map(PosLiteral(_))
        val (posLiterals, posNegLiterals) = allPosLiterals.partition(model.contains)
        val negLiterals = posNegLiterals.map(_.neg)
        val conflictLiterals = posLiterals ++ negLiterals
        println(conflictLiterals)
        Left(conflictLiterals)
    }
  }

  def solve(variables: Set[B], constraints: Set[Constraint[B]]): Either[Set[Constraint[B]], Map[B, Int]] = {
    sealed trait ConstraintVertex
    case object SourceVertex extends ConstraintVertex
    case class VariableVertex(x: B) extends ConstraintVertex

    val vertices: Set[ConstraintVertex] = variables.map(VariableVertex).toSet[ConstraintVertex] + SourceVertex
    val edges: Set[Edge[ConstraintVertex]] =
      constraints.map({ case Constraint(x, y, n) => (VariableVertex(y), n, VariableVertex(x)) }) ++
        variables.map(x => (SourceVertex, 0, VariableVertex(x)))

    BellmanFord.bellmanFord(vertices, edges, SourceVertex) match {
      case Right(distance) =>
        Right(distance.flatMap({
          case (SourceVertex, _) => None
          case (VariableVertex(x), value) => Some(x -> value)
        }))
      case Left(cycle) =>
        Left(cycle.map({ case (VariableVertex(y), n, VariableVertex(x)) => Constraint(x, y, n) })) // TODO: what about cycle containing SourceVertex?
    }
  }

  def extractVariables(constraints: Set[Constraint[B]]): Set[B] = {
    constraints.flatMap({ case Constraint(x, y, n) => Seq(x, y) })
  }
}
