package eu.sim642.algosmt.theory

object CoreTheory extends Theory {
  override def leftAssocFuncs: Set[String] = Set("and", "or")
  override def chainableFuncs: Set[String] = Set("=")
  override def pairwiseFuncs: Set[String] = Set("distinct")
}
