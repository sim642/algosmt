package eu.sim642.algosmt.idl

import eu.sim642.algosmt.bool.{BExp, BExpParser, Var}
import eu.sim642.algosmt.idl.IDL.Constraint
import eu.sim642.algosmt.smtlib.{Application, Atom, SExp}

object IDLConstraintBExpParser extends BExpParser[Constraint[String]] {

  object IntAtom {
    def unapply(sexp: SExp): Option[Int] = sexp match {
      case Atom(n) => Some(n.toInt)
      case Application("-", Atom(n)) => Some(-n.toInt)
      case _ => None
    }
  }

  override def fromSExp(sexp: SExp): BExp[Constraint[String]] = sexp match {
    case Application("<=", Application("-", Atom(x), Atom(y)), IntAtom(n)) => Var(Constraint(x, y, n))
    // TODO: add equivalent forms
  }
}
