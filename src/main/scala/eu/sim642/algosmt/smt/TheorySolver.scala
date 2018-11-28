package eu.sim642.algosmt.smt

import eu.sim642.algosmt.Literal

trait TheorySolver[A, B, C] {
  def solve(model: Set[Literal[A]]): Option[Map[B, C]]
}
