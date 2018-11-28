package eu.sim642.algosmt.idl

import eu.sim642.algosmt.idl.IDL.Constraint
import eu.sim642.algosmt.smt.TheorySolver
import eu.sim642.algosmt.{Literal, NegLiteral, PosLiteral}

class IDLTheorySolver[B] extends TheorySolver[Constraint[B], B, Int] {
  override def solve(model: Set[Literal[Constraint[B]]]): Option[Map[B, Int]] = {
    val constraints: Seq[Constraint[B]] = model.map({
      case PosLiteral(constraint) => constraint
      case NegLiteral(Constraint(x, y, n)) => Constraint(y, x, -n - 1) // !(x - y <= n) -> x - y > n -> y - x < -n -> y - x <= -n - 1
    }).toSeq
    val variables = IDL.extractVariables(constraints)
    IDL.solve(variables, constraints)
  }
}
