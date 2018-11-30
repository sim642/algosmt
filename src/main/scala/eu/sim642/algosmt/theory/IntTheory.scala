package eu.sim642.algosmt.theory

object IntTheory extends Theory {
  override def leftAssocFuncs: Set[String] = Set()
  override def chainableFuncs: Set[String] = Set("<=", "<", ">=", ">")
  override def pairwiseFuncs: Set[String] = Set()
}
