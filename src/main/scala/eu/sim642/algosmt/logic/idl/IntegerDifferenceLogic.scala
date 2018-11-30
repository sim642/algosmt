package eu.sim642.algosmt.logic.idl

import eu.sim642.algosmt.bool.BExpParser
import eu.sim642.algosmt.logic.Logic
import eu.sim642.algosmt.smt.LogicSolver
import eu.sim642.algosmt.theory.{CoreTheory, IntTheory, Theory}

object IntegerDifferenceLogic extends Logic[Constraint[String], String, Int] {
  override def theory: Theory = CoreTheory + IntTheory
  override def parser: BExpParser[Constraint[String]] = BExpParser.idlBooleanParser
  override def solver: LogicSolver[Constraint[String], String, Int] = new IDLSolver[String]
}
