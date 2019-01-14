package eu.sim642.algosmt.theory

object CoreTheory extends Theory {
  override val leftAssocFuncs: Set[String] = Set("and", "or", "xor")
  override val chainableFuncs: Set[String] = Set("=")
  override val pairwiseFuncs: Set[String] = Set("distinct")
}
