package eu.sim642.algosmt.logic.idl

import eu.sim642.algosmt.core.{BExpParser, CoreParser}
import eu.sim642.algosmt.logic.Logic
import eu.sim642.algosmt.logic.idl.IDLParser.IntAtom
import eu.sim642.algosmt.smt.LogicSolver
import eu.sim642.algosmt.smtlib.{Application, SExp}
import eu.sim642.algosmt.theory.{CoreTheory, IntTheory, Theory}

object IntegerDifferenceLogic extends Logic[Constraint[String], String, Int] {
  override def theory: Theory = CoreTheory + IntTheory
  override def parser: BExpParser[Constraint[String]] = new CoreParser(IDLParser)
  override def solver: LogicSolver[Constraint[String], String, Int] = new IDLSolver[String]

  override def toSExp(variable: String, value: Int): SExp = Application(variable, IntAtom(value))
}
