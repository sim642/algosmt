package eu.sim642.algosmt.idl

import eu.sim642.algosmt.bool.{BExp, BExpParser, Var}
import eu.sim642.algosmt.idl.IDL.Constraint
import eu.sim642.algosmt.smtlib.{Application, Atom, SExp}

object IDLConstraintBExpParser extends BExpParser[Constraint[String]] {
  override def fromSExp(sexp: SExp): BExp[Constraint[String]] = sexp match {
    case Application("<=", Application("-", Atom(x), Atom(y)), Atom(n)) => Var(Constraint(x, y, n.toInt))
    case Application("<=", Application("-", Atom(x), Atom(y)), Application("-", Atom(n))) => Var(Constraint(x, y, -n.toInt))
    // TODO: add equivalent forms
  }
}
