package eu.sim642.algosmt.core

import eu.sim642.algosmt.smtlib.{Application, SExp}

class CoreParser[A](delegate: BExpParser[A]) extends BExpParser[A] {
  def fromSExp(sexp: SExp): BExp[A] = sexp match {
    case Application("not", exp) => Not(fromSExp(exp))
    case Application("and", left, right) => And(fromSExp(left), fromSExp(right))
    case Application("or", left, right) => Or(fromSExp(left), fromSExp(right))
    case Application("xor", left, right) =>
      val leftBexp = fromSExp(left)
      val rightBexp = fromSExp(right)
      And(Or(leftBexp, rightBexp), Or(Not(leftBexp), Not(rightBexp))) // TODO: add Xor as BExp and split in CNFConverter instead?
    case exp => delegate.fromSExp(exp)
  }
}
