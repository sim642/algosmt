package eu.sim642.algosmt.smt

import eu.sim642.algosmt.smt.cnf.{CNF, Literal, NegLiteral, PosLiteral, extractVariables}
import eu.sim642.algosmt.util.HeadIterator

class BruteForceSMTSolver[A, B, C](logicSolver: LogicSolver[A, B, C]) extends SMTSolver[A, B, C] {

  def evaluate(cnf: CNF[A], literals: Set[Literal[A]]): Boolean = {
    cnf.forall(_.exists(literals.contains))
  }

  override def solve(cnf: CNF[A]): Option[Map[B, C]] = {
    val variables = extractVariables(cnf)
    val goodLiterals = for {
      posVariables <- variables.subsets
      posLiterals = posVariables.map(PosLiteral[A])
      negLiterals = (variables -- posVariables).map(NegLiteral[A])
      literals = (posLiterals ++ negLiterals).toSet[Literal[A]]
      if evaluate(cnf, literals)
    } yield literals
    goodLiterals.flatMap(logicSolver.solve(_, Map.empty)).headOption
  }
}
