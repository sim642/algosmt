package eu.sim642.algosmt.smt

import eu.sim642.algosmt.Literal
import eu.sim642.algosmt.SimSat.{CNF, extractVariables, findPure, simplify, toLiteral}

class DPLLSMTSolver[A, B, C](theorySolver: TheorySolver[A, B, C]) extends SMTSolver[A, B, C] {

  override def solve(cnf: CNF[A]): Option[Map[B, C]] = solve(cnf, Set.empty)

  private def solve(cnf: CNF[A], literals: Set[Literal[A]]): Option[Map[B, C]] = {
    if (cnf.isEmpty)
      return theorySolver.solve(literals)
    else if (cnf.exists(_.isEmpty))
      return None

    // unit propagation
    cnf.find(_.size == 1).map(_.head) match {
      case Some(unitLiteral) =>
        return solve(simplify(cnf, literals + unitLiteral), literals + unitLiteral)
      case None =>
    }

    // pure propagation not correct in SMT solving
    /*// pure propagation
    findPure(cnf) match {
      case Some(pureLiteral) =>
        return solve(simplify(cnf, literals + pureLiteral), literals + pureLiteral)
      case None =>
    }*/

    // splitting
    val variable = extractVariables(cnf).head
    solve(simplify(cnf, literals + variable), literals + variable) orElse solve(simplify(cnf, literals + variable.neg), literals + variable.neg)
  }
}
