package eu.sim642.algosmt.smt.cnf

sealed trait Literal[A] {
  val variable: A
  def neg: Literal[A]
}

case class PosLiteral[A](variable: A) extends Literal[A] {
  override def neg: Literal[A] = NegLiteral(variable)
  override def toString: String = s"$variable"
}

case class NegLiteral[A](variable: A) extends Literal[A] {
  override def neg: Literal[A] = PosLiteral(variable)
  override def toString: String = s"-$variable"
}
