package eu.sim642.algosmt.logic.idl0

import eu.sim642.algosmt.core.{BExp, BExpParser}
import eu.sim642.algosmt.logic.idl.IDLParser.IntAtom
import eu.sim642.algosmt.logic.idl.{Constraint, IDLParser}
import eu.sim642.algosmt.smtlib.{Application, Atom, SExp}

object IDL0Parser extends BExpParser[Constraint[String]] {

  def preProcess(sexp: SExp): SExp = sexp match {
    case Application(op, Atom(x), IntAtom(n)) => Application(op, Application("-", Atom(x), Atom(zeroVariable)), IntAtom(n))
    case Application(op, IntAtom(n), Atom(x)) => Application(op, IntAtom(n), Application("-", Atom(x), Atom(zeroVariable)))
    case _ => sexp
  }

  override def fromSExp(sexp: SExp): BExp[Constraint[String]] = IDLParser.fromSExp(preProcess(sexp))
}
