package eu.sim642.algosmt.theory

class AggregateTheory(theory1: Theory, theory2: Theory) extends Theory {
  override val leftAssocFuncs: Set[String] = theory1.leftAssocFuncs ++ theory2.leftAssocFuncs
  override val chainableFuncs: Set[String] = theory1.chainableFuncs ++ theory2.chainableFuncs
  override val pairwiseFuncs: Set[String] = theory1.pairwiseFuncs ++ theory2.pairwiseFuncs
}
