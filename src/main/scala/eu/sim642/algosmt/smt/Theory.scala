package eu.sim642.algosmt.smt

trait Theory {
  def leftAssocFuncs: Set[String]
  def chainableFuncs: Set[String]
  def pairwiseFuncs: Set[String]

  def +(that: Theory): AggregateTheory = new AggregateTheory(this, that)
}

object CoreTheory extends Theory {
  override def leftAssocFuncs: Set[String] = Set("and", "or")
  override def chainableFuncs: Set[String] = Set("=")
  override def pairwiseFuncs: Set[String] = Set("distinct")
}

object IntTheory extends Theory {
  override def leftAssocFuncs: Set[String] = Set()
  override def chainableFuncs: Set[String] = Set("<=", "<", ">=", ">")
  override def pairwiseFuncs: Set[String] = Set()
}

class AggregateTheory(theory1: Theory, theory2: Theory) extends Theory {
  override def leftAssocFuncs: Set[String] = theory1.leftAssocFuncs ++ theory2.leftAssocFuncs
  override def chainableFuncs: Set[String] = theory1.chainableFuncs ++ theory2.chainableFuncs
  override def pairwiseFuncs: Set[String] = theory1.pairwiseFuncs ++ theory2.pairwiseFuncs
}
