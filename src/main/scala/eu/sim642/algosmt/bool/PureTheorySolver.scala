package eu.sim642.algosmt.bool

import eu.sim642.algosmt.smt.TheorySolver
import eu.sim642.algosmt.{Literal, NegLiteral, PosLiteral}

class PureTheorySolver[B] extends TheorySolver[B, B, Boolean] {
  override def solve(model: Set[Literal[B]]): Option[Map[B, Boolean]] = {
    Some(model.map({
      case PosLiteral(variable) => variable -> true
      case NegLiteral(variable) => variable -> false
    }).toMap)
  }
}
