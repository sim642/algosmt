package eu.sim642.algosmt.smt

import eu.sim642.algosmt.smt.cnf.{CNF, Literal, extractVariables, toLiteral}

import scala.annotation.tailrec

class DPLLSMTSolver[A, B, C](logicSolver: LogicSolver[A, B, C]) extends SMTSolver[A, B, C] {

  override def solve(cnf: CNF[A]): Option[Map[B, C]] = checkedSolveUnit(cnf, Set.empty, Map.empty)

  private def checkedSolve(solveFunction: (CNF[A], Model[A], Map[B, C]) => Option[Map[B, C]])(cnf: CNF[A], model: Model[A], prevLogicModel: Map[B, C]): Option[Map[B, C]] = {
    if (cnf.isEmpty)
      return logicSolver.solve(model, prevLogicModel)
    else if (cnf.exists(_.isEmpty))
      return None

    val logicModelOption = logicSolver.solve(model, prevLogicModel)
    if (logicModelOption.isEmpty)
      return None

    solveFunction(cnf, model, logicModelOption.get)
  }

  private def propagate(cnf: CNF[A], literal: Literal[A]): CNF[A] = {
    val negLiteral = literal.neg
    cnf.filterNot(_.contains(literal)).map(_ - negLiteral)
  }

  private val checkedSolveUnit: (CNF[A], Model[A], Map[B, C]) => Option[Map[B, C]] = checkedSolve(solveUnit(_, _, _))

  @tailrec
  private def solveUnit(cnf: CNF[A], model: Model[A], prevLogicModel: Map[B, C], unitCount: Int = 0): Option[Map[B, C]] = {
    // unit propagation
    cnf.find(_.size == 1).map(_.head) match {
      case Some(unitLiteral) =>
        solveUnit(propagate(cnf, unitLiteral), model + unitLiteral, prevLogicModel, unitCount + 1)
      case None =>
        if (unitCount > 0)
          checkedSolveSplit(cnf, model, prevLogicModel)
        else
          solveSplit(cnf, model, prevLogicModel)
    }
  }

  private val checkedSolveSplit: (CNF[A], Model[A], Map[B, C]) => Option[Map[B, C]] = checkedSolve(solveSplit)

  private def solveSplit(cnf: CNF[A], model: Model[A], prevLogicModel: Map[B, C]): Option[Map[B, C]] = {
    // splitting
    val variable = extractVariables(cnf).head
    checkedSolveUnit(propagate(cnf, variable), model + variable, prevLogicModel) orElse checkedSolveUnit(propagate(cnf, variable.neg), model + variable.neg, prevLogicModel)
  }
}
