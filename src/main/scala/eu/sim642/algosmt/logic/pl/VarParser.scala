package eu.sim642.algosmt.logic.pl

import eu.sim642.algosmt.core.{BExp, BExpParser, Var}
import eu.sim642.algosmt.smtlib.{Atom, SExp}

object VarParser extends BExpParser[String] {
  override def fromSExp(sexp: SExp): BExp[String] = sexp match {
    case Atom(str) => Var(str)
  }
}
