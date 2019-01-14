package eu.sim642.algosmt.logic.idl0

import eu.sim642.algosmt.core.{BExpParser, CoreParser}
import eu.sim642.algosmt.logic.Logic
import eu.sim642.algosmt.logic.idl.IDLParser.IntAtom
import eu.sim642.algosmt.logic.idl._
import eu.sim642.algosmt.smt.LogicSolver
import eu.sim642.algosmt.smtlib.{Application, SExp}
import eu.sim642.algosmt.theory.{CoreTheory, IntTheory, Theory}

object IntegerDifferenceLogic0 extends Logic[Constraint[String], String, Int] {
  override def theory: Theory = CoreTheory + IntTheory
  override def parser: BExpParser[Constraint[String]] = new CoreParser(IDL0Parser)
  override def solver: LogicSolver[Constraint[String], String, Int] = new IDL0Solver

  override def toSExp(variable: String, value: Int): SExp = Application(variable, IntAtom(value))
}
