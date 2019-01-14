package eu.sim642.algosmt.logic.idl0

import eu.sim642.algosmt.logic.idl.{Constraint, IDLSolver}
import eu.sim642.algosmt.smt.Model

class IDL0Solver extends IDLSolver[String] {
  override def solve(model: Model[Constraint[String]], prevLogicModel: Map[String, Int]): Option[Map[String, Int]] = {
    super.solve(model, prevLogicModel).map({ solution =>
      if (solution.contains(zeroVariable)) {
        val zero = solution(zeroVariable)
        solution.mapValues(_ - zero) - zeroVariable
      }
      else
        solution
    })
  }
}
