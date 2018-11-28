package eu.sim642.algosmt.smt

import eu.sim642.algosmt.{Literal, NegLiteral, PosLiteral}
import eu.sim642.algosmt.SimSat.{CNF, evaluate, extractVariables, HeadIterator, toLiteral}
import eu.sim642.algosmt.idl.IDL
import eu.sim642.algosmt.idl.IDL.Constraint

trait SMTSolver[A, B, C] {
  def solve(cnf: CNF[A]): Option[Map[B, C]]
}

trait TheorySolver[A, B, C] {
  def solve(model: Set[Literal[A]]): Option[Map[B, C]]
}

class PureTheorySolver[B] extends TheorySolver[B, B, Boolean] {
  override def solve(model: Set[Literal[B]]): Option[Map[B, Boolean]] = {
    Some(model.map({
      case PosLiteral(variable) => variable -> true
      case NegLiteral(variable) => variable -> false
    }).toMap)
  }
}

class IDLTheorySolver[B] extends TheorySolver[Constraint[B], B, Int] {
  override def solve(model: Set[Literal[Constraint[B]]]): Option[Map[B, Int]] = {
    val constraints: Seq[Constraint[B]] = model.map({
      case PosLiteral(constraint) => constraint
      case NegLiteral(Constraint(x, y, n)) => Constraint(y, x, -n - 1) // !(x - y <= n) -> x - y > n -> y - x < -n -> y - x <= -n - 1
    }).toSeq
    val variables = IDL.extractVariables(constraints)
    IDL.solve(variables, constraints)
  }
}

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

object SMTSolver {
  val idlBruteForceSolver = new BruteForceSMTSolver(new IDLTheorySolver[String])

  def main(args: Array[String]): Unit = {
    println(idlBruteForceSolver.solve(List(
      List(Constraint("x1", "x2", 2)),
      List(Constraint("x2", "x3", 1)),
      List(Constraint("x3", "x1", -4), Constraint("x3", "x1", -1)),
    )))
  }
}
