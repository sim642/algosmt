package eu.sim642.algosmt.theory

class AggregateTheory(theory1: Theory, theory2: Theory) extends Theory {
  override def leftAssocFuncs: Set[String] = theory1.leftAssocFuncs ++ theory2.leftAssocFuncs
  override def chainableFuncs: Set[String] = theory1.chainableFuncs ++ theory2.chainableFuncs
  override def pairwiseFuncs: Set[String] = theory1.pairwiseFuncs ++ theory2.pairwiseFuncs
}
