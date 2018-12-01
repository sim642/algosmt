package eu.sim642.algosmt.smt

import scala.language.implicitConversions

package object cnf {
  // TODO: use Sets?
  type Disjunct[A] = Seq[Literal[A]]
  type CNF[A] = Seq[Disjunct[A]]

  def extractVariables[A](cnf: CNF[A]): Set[A] = {
    (for {
      clause <- cnf
      literal <- clause
    } yield literal.variable).toSet
  }

  implicit def toLiteral[A](variable: A): PosLiteral[A] = PosLiteral(variable)
}
