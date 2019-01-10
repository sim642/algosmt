package eu.sim642.algosmt.logic.pl

import eu.sim642.algosmt.smt.{LogicSolver, Model}
import eu.sim642.algosmt.smt.cnf.{NegLiteral, PosLiteral}

class PropositionalSolver[B] extends LogicSolver[B, B, Boolean] {
  override def solve(model: Model[B], prevLogicModel: Map[B, Boolean]): Option[Map[B, Boolean]] = {
    Some(model.map({
      case PosLiteral(variable) => variable -> true
      case NegLiteral(variable) => variable -> false
    }).toMap)
  }
}
