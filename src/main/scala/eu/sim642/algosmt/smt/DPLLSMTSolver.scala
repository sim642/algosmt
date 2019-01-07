package eu.sim642.algosmt.smt

import eu.sim642.algosmt.smt.cnf.{CNF, Literal, extractVariables, toLiteral}

import scala.annotation.tailrec

class DPLLSMTSolver[A, B, C](logicSolver: LogicSolver[A, B, C]) extends SMTSolver[A, B, C] {

  override def solve(cnf: CNF[A]): Option[Map[B, C]] = solveUnitFull(cnf, Set.empty)

  private def propagate(cnf: CNF[A], literal: Literal[A]): CNF[A] = {
    val negLiteral = literal.neg
    cnf.filterNot(_.contains(literal)).map(_ - negLiteral)
  }

  private def solveUnitFull(cnf: CNF[A], model: Model[A]): Option[Map[B, C]] = {
    if (cnf.isEmpty)
      return logicSolver.solve(model)
    else if (cnf.exists(_.isEmpty))
      return None
    else if (logicSolver.solve(model).isEmpty)
      return None

    solveUnit(cnf, model)
  }

  @tailrec
  private def solveUnit(cnf: CNF[A], model: Model[A], unitCount: Int = 0): Option[Map[B, C]] = {
    // unit propagation
    cnf.find(_.size == 1).map(_.head) match {
      case Some(unitLiteral) =>
        solveUnit(propagate(cnf, unitLiteral), model + unitLiteral, unitCount + 1)
      case None =>
        if (unitCount > 0)
          solveSplitFull(cnf, model)
        else
          solveSplit(cnf, model)
    }
  }

  private def solveSplitFull(cnf: CNF[A], model: Model[A]): Option[Map[B, C]] = {
    // TODO: remove duplication with solveUnitFull
    if (cnf.isEmpty)
      return logicSolver.solve(model)
    else if (cnf.exists(_.isEmpty))
      return None
    else if (logicSolver.solve(model).isEmpty)
      return None

    solveSplit(cnf, model)
  }

  private def solveSplit(cnf: CNF[A], model: Model[A]): Option[Map[B, C]] = {
    // splitting
    val variable = extractVariables(cnf).head
    solveUnitFull(propagate(cnf, variable), model + variable) orElse solveUnitFull(propagate(cnf, variable.neg), model + variable.neg)
  }
}
