package eu.sim642.algosmt.smt

import eu.sim642.algosmt.smt.cnf.{CNF, Literal, extractVariables, toLiteral}

class DPLLSMTSolver[A, B, C](logicSolver: LogicSolver[A, B, C]) extends SMTSolver[A, B, C] {

  override def solve(cnf: CNF[A]): Option[Map[B, C]] = solve(cnf, Set.empty)

  private def propagate(cnf: CNF[A], literal: Literal[A]): CNF[A] = {
    val negLiteral = literal.neg
    cnf.filterNot(_.contains(literal)).map(_ - negLiteral)
  }

  private def solve(cnf: CNF[A], model: Model[A]): Option[Map[B, C]] = {
    if (cnf.isEmpty)
      return logicSolver.solve(model)
    else if (cnf.exists(_.isEmpty))
      return None
    else if (logicSolver.solve(model).isEmpty)
      return None

    // unit propagation
    cnf.find(_.size == 1).map(_.head) match {
      case Some(unitLiteral) =>
        return solve(propagate(cnf, unitLiteral), model + unitLiteral)
      case None =>
    }

    // splitting
    val variable = extractVariables(cnf).head
    solve(propagate(cnf, variable), model + variable) orElse solve(propagate(cnf, variable.neg), model + variable.neg)
  }
}
