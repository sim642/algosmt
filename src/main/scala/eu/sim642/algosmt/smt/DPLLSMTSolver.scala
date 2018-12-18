package eu.sim642.algosmt.smt

import eu.sim642.algosmt.smt.cnf.{CNF, Literal, extractVariables, toLiteral}

class DPLLSMTSolver[A, B, C](logicSolver: LogicSolver[A, B, C]) extends SMTSolver[A, B, C] {

  override def solve(cnf: CNF[A]): Option[Map[B, C]] = solve(cnf, Set.empty)

  def simplify(cnf: CNF[A], literals: Set[Literal[A]]): CNF[A] = {
    cnf.filterNot(_.exists(literal => literals.contains(literal))).map(_.filterNot(literal => literals.contains(literal.neg)))
  }

  def findPure(cnf: CNF[A]): Option[Literal[A]] = {
    val literals = cnf.flatten.toSet
    val negLiterals = literals.map(_.neg)
    val symdiff = (literals diff negLiterals) ++ (negLiterals diff literals)
    symdiff.headOption
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
        return solve(simplify(cnf, model + unitLiteral), model + unitLiteral)
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
    solve(simplify(cnf, model + variable), model + variable) orElse solve(simplify(cnf, model + variable.neg), model + variable.neg)
  }
}
