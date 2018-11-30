package eu.sim642.algosmt.logic

import eu.sim642.algosmt.core.BExpParser
import eu.sim642.algosmt.smt.LogicSolver
import eu.sim642.algosmt.smtlib.SExp
import eu.sim642.algosmt.theory.Theory

trait Logic[A, B, C] {
  def theory: Theory
  def parser: BExpParser[A]
  def solver: LogicSolver[A, B, C]
  def toSExp(variable: B, value: C): SExp
}
