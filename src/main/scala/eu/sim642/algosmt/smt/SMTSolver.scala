package eu.sim642.algosmt.smt

import eu.sim642.algosmt.SimSat.{CNF, toLiteral}
import eu.sim642.algosmt.bool.PureTheorySolver
import eu.sim642.algosmt.idl.IDL.Constraint
import eu.sim642.algosmt.idl.IDLTheorySolver

trait SMTSolver[A, B, C] {
  def solve(cnf: CNF[A]): Option[Map[B, C]]
}

object SMTSolver {
  val pureBruteForceSolver = new BruteForceSMTSolver(new PureTheorySolver[String])
  val idlBruteForceSolver = new BruteForceSMTSolver(new IDLTheorySolver[String])

  def main(args: Array[String]): Unit = {
    println(idlBruteForceSolver.solve(List(
      List(Constraint("x1", "x2", 2)),
      List(Constraint("x2", "x3", 1)),
      List(Constraint("x3", "x1", -4), Constraint("x3", "x1", -1)),
    )))
  }
}
