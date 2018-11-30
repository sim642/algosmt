package eu.sim642.algosmt.theory

trait Theory {
  def leftAssocFuncs: Set[String]
  def chainableFuncs: Set[String]
  def pairwiseFuncs: Set[String]

  def +(that: Theory): AggregateTheory = new AggregateTheory(this, that)
}
