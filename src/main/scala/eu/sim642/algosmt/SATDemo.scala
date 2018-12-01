package eu.sim642.algosmt

import eu.sim642.algosmt.logic.pl.PropositionalSolver
import eu.sim642.algosmt.smt.cnf.{CNF, Literal, toLiteral}
import eu.sim642.algosmt.smt.{BruteForceSMTSolver, DPLLSMTSolver}
import eu.sim642.algosmt.util.StreamUnfoldOps

object SATDemo {

  def solveAll[A](satSolve: CNF[A] => Option[Set[Literal[A]]])(cnf: CNF[A]): List[Set[Literal[A]]] = {
    Stream.unfold(cnf) { cnf =>
      satSolve(cnf).map(solution => (solution.map(_.neg).toList +: cnf, solution))
    }.toList
  }

  def main(args: Array[String]): Unit = {
    val bruteForce = new BruteForceSMTSolver(new PropositionalSolver[Char]).solve(_)
    val dpll = new DPLLSMTSolver(new PropositionalSolver[Char]).solve(_)

    val cnf1: CNF[Char] = List(
      List('P', 'Q', 'R'.neg),
      List('P', 'Q'.neg),
      List('P'.neg),
      List('R'),
      List('U')
    )

    val cnf2: CNF[Char] = List(
      List('P', 'Q'),
      List('Q'.neg),
      List('P'.neg, 'Q', 'R'.neg)
    )

    val cnf3: CNF[Char] = List(
      List('P', 'Q'),
      List('P', 'Q'.neg),
      List('P'.neg, 'Q'),
      List('P'.neg, 'R'.neg),
    )

    val cnf4: CNF[Char] = List(
      List('P', 'Q'.neg),
      List('P'.neg, 'R'),
      List('Q')
    )

    val cnf5: CNF[Char] = List(
      List('p', 'q'),
      List('p'.neg, 'q'),
      List('r'.neg, 'q'.neg),
      List('r', 'q'.neg),
    )

    val cnf6: CNF[Char] = List(
      List('p', 'q', 'r'),
      List('p'.neg, 'q'.neg, 'r'.neg),
      List('p'.neg, 'q', 'r'),
      List('q'.neg, 'r'),
      List('q', 'r'.neg),
    )

    val cnf7: CNF[Char] = List(
      List('q'.neg, 'p'),
      List('p'.neg, 'q'.neg),
      List('q', 'r'),
      List('q'.neg, 'r'.neg),
      List('p'.neg, 'r'.neg),
      List('p', 'r'.neg)
    )

    println("bruteForce")
    println(bruteForce(cnf1))
    println(bruteForce(cnf2))
    println(bruteForce(cnf3))
    println(bruteForce(cnf4))
    println(bruteForce(cnf5))
    println(bruteForce(cnf6))
    println(bruteForce(cnf7))
    /*println(solveAll[Char](bruteForce)(List(
      List('P', 'P'.neg)
    )))*/

    println("dpll")
    println(dpll(cnf1))
    println(dpll(cnf2))
    println(dpll(cnf3))
    println(dpll(cnf4))
    println(dpll(cnf5))
    println(dpll(cnf6))
    println(dpll(cnf7))
    /*println(solveAll[Char](dpll(_))(List(
      List('P', 'P'.neg)
    )))*/
  }
}
