package eu.sim642.algosmt.smt

import scala.language.implicitConversions

package object cnf {
  type Disjunct[A] = Set[Literal[A]]
  type CNF[A] = Set[Disjunct[A]]

  def extractVariables[A](cnf: CNF[A]): Set[A] = {
    for {
      clause <- cnf
      literal <- clause
    } yield literal.variable
  }

  implicit def toLiteral[A](variable: A): PosLiteral[A] = PosLiteral(variable)
}
