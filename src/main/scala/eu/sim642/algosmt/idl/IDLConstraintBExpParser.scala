package eu.sim642.algosmt.idl

import eu.sim642.algosmt.bool._
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
    case Application("<=", Application("-", Atom(x), Atom(y)), IntAtom(n)) =>
      Var(Constraint(x, y, n)) // x - y <= n
    case Application("<", Application("-", Atom(x), Atom(y)), IntAtom(n)) =>
      Var(Constraint(x, y, n - 1)) // x - y < n -> x - y <= n - 1
    case Application(">=", Application("-", Atom(x), Atom(y)), IntAtom(n)) =>
      Var(Constraint(y, x, -n)) // x - y >= n -> y - x <= -n
    case Application(">", Application("-", Atom(x), Atom(y)), IntAtom(n)) =>
      Var(Constraint(y, x, -n - 1)) // x - y > n -> y - x < -n -> y - x <= -n - 1
    case Application("=", Application("-", Atom(x), Atom(y)), IntAtom(n)) =>
      And(Var(Constraint(x, y, n)), Var(Constraint(y, x, -n))) // x - y = n -> x - y <= n && x - y >= n -> x - y <= n && y - x <= -n
    case Application("distinct", Application("-", Atom(x), Atom(y)), IntAtom(n)) =>
      Or(Var(Constraint(x, y, n - 1)), Var(Constraint(y, x, -n - 1))) // x - y ≠ n -> x - y < n || x - y > n -> x - y <= n - 1 || y - x <= -n - 1
    // TODO: add equivalent forms
  }
}
