package eu.sim642.algosmt.logic.pl

import eu.sim642.algosmt.core.{BExpParser, CoreParser}
import eu.sim642.algosmt.logic.Logic
import eu.sim642.algosmt.smt.LogicSolver
import eu.sim642.algosmt.smtlib.{Application, Atom, SExp}
import eu.sim642.algosmt.theory.{CoreTheory, Theory}

object PropositionalLogic extends Logic[String, String, Boolean] {
  override def theory: Theory = CoreTheory
  override def parser: BExpParser[String] = new CoreParser(VarParser)
  override def solver: LogicSolver[String, String, Boolean] = new PropositionalSolver[String]

  override def toSExp(variable: String, value: Boolean): SExp = Application(variable, Atom(value.toString))
}
