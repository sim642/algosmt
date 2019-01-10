package eu.sim642.algosmt.logic.idl

import eu.sim642.algosmt.core.{BExp, BExpParser, CoreParser}
import eu.sim642.algosmt.logic.Logic
import eu.sim642.algosmt.logic.idl.IDLParser.IntAtom
import eu.sim642.algosmt.smt.{LogicSolver, Model}
import eu.sim642.algosmt.smtlib.{Application, Atom, SExp}
import eu.sim642.algosmt.theory.{CoreTheory, IntTheory, Theory}

object IDLParser2 extends BExpParser[Constraint[String]] {

  def preProcess(sexp: SExp): SExp = sexp match {
    case Application(op, Atom(x), IntAtom(n)) => Application(op, Application("-", Atom(x), Atom("Z")), IntAtom(n))
    case Application(op, IntAtom(n), Atom(x)) => Application(op, IntAtom(n), Application("-", Atom(x), Atom("Z")))
    case _ => sexp
  }

  override def fromSExp(sexp: SExp): BExp[Constraint[String]] = IDLParser.fromSExp(preProcess(sexp))
}

class IDLSolver2 extends IDLSolver[String] {
  override def solve(model: Model[Constraint[String]], prevLogicModel: Map[String, Int]): Option[Map[String, Int]] = {
    super.solve(model, prevLogicModel).map({ solution =>
      if (solution.contains("Z")) {
        val zero = solution("Z")
        solution.mapValues(_ - zero) - "Z"
      }
      else
        solution
    })
  }
}

object IntegerDifferenceLogic2 extends Logic[Constraint[String], String, Int] {
  override def theory: Theory = CoreTheory + IntTheory
  override def parser: BExpParser[Constraint[String]] = new CoreParser(IDLParser2)
  override def solver: LogicSolver[Constraint[String], String, Int] = new IDLSolver2

  override def toSExp(variable: String, value: Int): SExp = Application(variable, IntAtom(value))
}
