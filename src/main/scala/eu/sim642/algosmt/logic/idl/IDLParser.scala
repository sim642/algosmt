package eu.sim642.algosmt.logic.idl

import eu.sim642.algosmt.core._
import eu.sim642.algosmt.smtlib.{Application, Atom, SExp}

import scala.util.Try

object IDLParser extends BExpParser[Constraint[String]] {

  object IntAtom {
    def apply(n: Int): SExp = {
      if (n < 0)
        Application("-", Atom((-n).toString))
      else
        Atom(n.toString)
    }

    def unapply(sexp: SExp): Option[Int] = sexp match {
      case Atom(n) => Try(n.toInt).toOption
      case Application("-", Atom(n)) => Try(-n.toInt).toOption
      case _ => None
    }
  }

  object OperatorConstraint {
    def unapply(sexp: SExp): Option[(String, String, String, Int)] = sexp match {
      case Application(op, Application("-", Atom(x), Atom(y)), IntAtom(n)) => Some((op, x, y, n)) // x - y OP n
      case Application(op, IntAtom(n), Application("-", Atom(x), Atom(y))) => Some((op, y, x, -n)) // n OP x - y -> y - x OP -n
      case Application(op, Atom(x), Application("+", Atom(y), IntAtom(n))) => Some((op, x, y, n)) // x OP y + n -> x - y OP n
      case Application(op, Atom(x), Application("-", Atom(y), IntAtom(n))) => Some((op, x, y, -n)) // x OP y - n -> x - y OP -n
      case Application(op, Atom(x), Application("+", IntAtom(n), Atom(y))) => Some((op, x, y, n)) // x OP n + y -> x - y OP n
      case Application(op, Application("+", Atom(x), IntAtom(n)), Atom(y)) => Some((op, x, y, -n)) // x + n OP y -> x - y OP -n
      case Application(op, Application("-", Atom(x), IntAtom(n)), Atom(y)) => Some((op, x, y, n)) // x - n OP y -> x - y OP n
      case Application(op, Application("+", IntAtom(n), Atom(x)), Atom(y)) => Some((op, x, y, -n)) // n + x OP y -> x - y OP -n
      case Application(op, Atom(x), Atom(y)) => Some((op, x, y, 0)) // x OP y -> x - y OP 0
      case _ => None
    }
  }

  override def fromSExp(sexp: SExp): BExp[Constraint[String]] = sexp match {
    case OperatorConstraint("<=", x, y, n) =>
      Var(Constraint(x, y, n)) // x - y <= n
    case OperatorConstraint("<", x, y, n) =>
      Var(Constraint(x, y, n - 1)) // x - y < n -> x - y <= n - 1
    case OperatorConstraint(">=", x, y, n) =>
      Var(Constraint(y, x, -n)) // x - y >= n -> y - x <= -n
    case OperatorConstraint(">", x, y, n) =>
      Var(Constraint(y, x, -n - 1)) // x - y > n -> y - x < -n -> y - x <= -n - 1
    case OperatorConstraint("=", x, y, n) =>
      And(Var(Constraint(x, y, n)), Var(Constraint(y, x, -n))) // x - y = n -> x - y <= n && x - y >= n -> x - y <= n && y - x <= -n
    case OperatorConstraint("distinct", x, y, n) =>
      Or(Var(Constraint(x, y, n - 1)), Var(Constraint(y, x, -n - 1))) // x - y ≠ n -> x - y < n || x - y > n -> x - y <= n - 1 || y - x <= -n - 1
  }
}
