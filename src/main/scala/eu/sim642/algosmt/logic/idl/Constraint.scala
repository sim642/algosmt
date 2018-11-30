package eu.sim642.algosmt.logic.idl

case class Constraint[A](x: A, y: A, n: Int) {
  override def toString: String = s"($x - $y ≤ $n)"
}
