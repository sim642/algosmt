package eu.sim642.algosmt

import eu.sim642.algosmt.smt.cnf.Literal

package object smt {
  type Model[A] = Set[Literal[A]]
}
