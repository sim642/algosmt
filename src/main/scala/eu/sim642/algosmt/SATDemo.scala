package eu.sim642.algosmt

import eu.sim642.algosmt.logic.pl.PropositionalSolver
import eu.sim642.algosmt.smt.cnf.{CNF, Literal, toLiteral}
import eu.sim642.algosmt.smt.{BruteForceSMTSolver, DPLLSMTSolver}
import eu.sim642.algosmt.util.StreamUnfoldOps

object SATDemo {

  def solveAll[A](satSolve: CNF[A] => Option[Set[Literal[A]]])(cnf: CNF[A]): List[Set[Literal[A]]] = {
    Stream.unfold(cnf) { cnf =>
      satSolve(cnf).map(solution => (cnf + solution.map(_.neg), solution))
    }.toList
  }

  def main(args: Array[String]): Unit = {
    val bruteForce = new BruteForceSMTSolver(new PropositionalSolver[Char]).solve(_)
    val dpll = new DPLLSMTSolver(new PropositionalSolver[Char]).solve(_)

    val cnf1: CNF[Char] = Set(
      Set('P', 'Q', 'R'.neg),
      Set('P', 'Q'.neg),
      Set('P'.neg),
      Set('R'),
      Set('U')
    )

    val cnf2: CNF[Char] = Set(
      Set('P', 'Q'),
      Set('Q'.neg),
      Set('P'.neg, 'Q', 'R'.neg)
    )

    val cnf3: CNF[Char] = Set(
      Set('P', 'Q'),
      Set('P', 'Q'.neg),
      Set('P'.neg, 'Q'),
      Set('P'.neg, 'R'.neg),
    )

    val cnf4: CNF[Char] = Set(
      Set('P', 'Q'.neg),
      Set('P'.neg, 'R'),
      Set('Q')
    )

    val cnf5: CNF[Char] = Set(
      Set('p', 'q'),
      Set('p'.neg, 'q'),
      Set('r'.neg, 'q'.neg),
      Set('r', 'q'.neg),
    )

    val cnf6: CNF[Char] = Set(
      Set('p', 'q', 'r'),
      Set('p'.neg, 'q'.neg, 'r'.neg),
      Set('p'.neg, 'q', 'r'),
      Set('q'.neg, 'r'),
      Set('q', 'r'.neg),
    )

    val cnf7: CNF[Char] = Set(
      Set('q'.neg, 'p'),
      Set('p'.neg, 'q'.neg),
      Set('q', 'r'),
      Set('q'.neg, 'r'.neg),
      Set('p'.neg, 'r'.neg),
      Set('p', 'r'.neg)
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
