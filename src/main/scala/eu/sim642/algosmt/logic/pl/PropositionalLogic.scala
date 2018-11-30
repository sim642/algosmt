package eu.sim642.algosmt.logic.pl

import eu.sim642.algosmt.bool.{BExpParser, PureLogicSolver}
import eu.sim642.algosmt.logic.Logic
import eu.sim642.algosmt.smt.LogicSolver
import eu.sim642.algosmt.theory.{CoreTheory, Theory}

object PropositionalLogic extends Logic[String, String, Boolean] {
  override def theory: Theory = CoreTheory
  override def parser: BExpParser[String] = BExpParser.pureBooleanParser
  override def solver: LogicSolver[String, String, Boolean] = new PureLogicSolver[String]
}
