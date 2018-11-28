package eu.sim642.algosmt.smt

import eu.sim642.algosmt.SimSat.{CNF, evaluate, extractVariables}
import eu.sim642.algosmt.{Literal, NegLiteral, PosLiteral}

import eu.sim642.algosmt.SimSat.HeadIterator

class BruteForceSMTSolver[A, B, C](theorySolver: TheorySolver[A, B, C]) extends SMTSolver[A, B, C] {

  override def solve(cnf: CNF[A]): Option[Map[B, C]] = {
    val variables = extractVariables(cnf)
    val goodLiterals = for {
      posVariables <- variables.subsets
      posLiterals = posVariables.map(PosLiteral[A])
      negLiterals = (variables -- posVariables).map(NegLiteral[A])
      literals = (posLiterals ++ negLiterals).toSet[Literal[A]]
      if evaluate(cnf, literals)
    } yield literals
    goodLiterals.flatMap(theorySolver.solve).headOption
  }
}
