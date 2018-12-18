package eu.sim642.algosmt.smt

import eu.sim642.algosmt.smt.cnf.{CNF, Literal, extractVariables, toLiteral}

class DPLLSMTSolver[A, B, C](logicSolver: LogicSolver[A, B, C]) extends SMTSolver[A, B, C] {

  override def solve(cnf: CNF[A]): Option[Map[B, C]] = solve(cnf, Set.empty)._1

  private def propagate(cnf: CNF[A], literal: Literal[A]): CNF[A] = {
    val negLiteral = literal.neg
    cnf.filterNot(_.contains(literal)).map(_ - negLiteral)
  }

  private def solve(cnf: CNF[A], model: Model[A], prefix: String = ""): (Option[Map[B, C]], CNF[A]) = {
    if (cnf.isEmpty) {
      println(prefix + "empty cnf")
      return logicSolver.solve(model) match {
        case Right(value) => (Some(value), Set.empty)
        case Left(conflict) =>
          println(prefix + s"learn: $conflict")
          (None, Set(conflict.map(_.neg)))
      }
    }
    else if (cnf.exists(_.isEmpty)) {
      println(prefix + "empty clause")
      return (None, Set.empty)
    }
    else {
      logicSolver.solve(model) match {
        case Right(value) =>
        case Left(conflict) =>
          println(prefix + "logic conflict")
          println(prefix + s"learn: $conflict")
          return (None, Set(conflict.map(_.neg)))
      }
    }

    // unit propagation
    cnf.find(_.size == 1).map(_.head) match {
      case Some(unitLiteral) =>
        println(prefix + s"unit: $unitLiteral")
        return solve(propagate(cnf, unitLiteral), model + unitLiteral, prefix + "| ")
      case None =>
    }

    // splitting
    val variable = extractVariables(cnf).head
    println(prefix + s"split: $variable")
    solve(propagate(cnf, variable), model + variable, prefix + "| ") match {
      case (Some(value), learned) => (Some(value), learned)
      case (None, learned) =>
        solve(propagate(cnf ++ learned, variable.neg), model + variable.neg, prefix + "| ")
    }
  }
}
