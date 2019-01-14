package eu.sim642.algosmt.theory

object IntTheory extends Theory {
  override val leftAssocFuncs: Set[String] = Set()
  override val chainableFuncs: Set[String] = Set("<=", "<", ">=", ">")
  override val pairwiseFuncs: Set[String] = Set()
}
