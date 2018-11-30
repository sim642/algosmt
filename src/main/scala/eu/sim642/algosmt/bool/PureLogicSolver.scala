package eu.sim642.algosmt.bool

import eu.sim642.algosmt.smt.{LogicSolver, Model}
import eu.sim642.algosmt.{NegLiteral, PosLiteral}

class PureLogicSolver[B] extends LogicSolver[B, B, Boolean] {
  override def solve(model: Model[B]): Option[Map[B, Boolean]] = {
    Some(model.map({
      case PosLiteral(variable) => variable -> true
      case NegLiteral(variable) => variable -> false
    }).toMap)
  }
}
