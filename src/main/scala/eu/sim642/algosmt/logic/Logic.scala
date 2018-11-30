package eu.sim642.algosmt.logic

import eu.sim642.algosmt.bool.BExpParser
import eu.sim642.algosmt.smt.TheorySolver
import eu.sim642.algosmt.theory.Theory

trait Logic[A, B, C] {
  def theory: Theory
  def parser: BExpParser[A]
  def solver: TheorySolver[A, B, C]
}
