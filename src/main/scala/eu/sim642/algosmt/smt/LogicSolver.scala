package eu.sim642.algosmt.smt

trait LogicSolver[A, B, C] {
  def solve(model: Model[A], prevLogicModel: Map[B, C]): Option[Map[B, C]]
}
