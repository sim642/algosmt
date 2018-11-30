package eu.sim642.algosmt.smt

trait LogicSolver[A, B, C] {
  def solve(model: Model[A]): Option[Map[B, C]]
}
