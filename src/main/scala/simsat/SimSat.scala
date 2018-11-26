package simsat

import scala.language.implicitConversions

sealed trait Literal[A] {
  val variable: A
  def neg: Literal[A]
}

case class PosLiteral[A](variable: A) extends Literal[A] {
  override def neg: Literal[A] = NegLiteral(variable)
  override def toString: String = s"$variable"
}

case class NegLiteral[A](variable: A) extends Literal[A] {
  override def neg: Literal[A] = PosLiteral(variable)
  override def toString: String = s"-$variable"
}

object SimSat {

  type Disjunct[A] = List[Literal[A]]
  type CNF[A] = List[Disjunct[A]]

  def extractVariables[A](cnf: CNF[A]): Set[A] = {
    (for {
      clause <- cnf
      literal <- clause
    } yield literal.variable).toSet
  }

  def evaluate[A](cnf: CNF[A], literals: Set[Literal[A]]): Boolean = {
    cnf.forall(_.exists(literals.contains))
  }

  implicit class HeadIterator[A](it: Iterator[A]) {
    def headOption: Option[A] = if (it.nonEmpty) Some(it.next) else None
  }

  def bruteForce[A](cnf: CNF[A]): Option[Set[Literal[A]]] = {
    val variables = extractVariables(cnf)
    val goodLiterals = for {
      posVariables <- variables.subsets
      posLiterals = posVariables.map(PosLiteral[A])
      negLiterals = (variables -- posVariables).map(NegLiteral[A])
      literals = (posLiterals ++ negLiterals).toSet[Literal[A]]
      if evaluate(cnf, literals)
    } yield literals
    goodLiterals.headOption
  }

  def simplify[A](cnf: CNF[A], literals: Set[Literal[A]]): CNF[A] = {
    cnf.filterNot(_.exists(literal => literals.contains(literal))).map(_.filterNot(literal => literals.contains(literal.neg)))
  }

  def findPure[A](cnf: CNF[A]): Option[Literal[A]] = {
    val literals = cnf.flatten.toSet
    val negLiterals = literals.map(_.neg)
    val symdiff = (literals diff negLiterals) ++ (negLiterals diff literals)
    symdiff.headOption
  }

  def dpll[A](cnf: CNF[A], literals: Set[Literal[A]] = Set.empty[Literal[A]]): Option[Set[Literal[A]]] = {
    if (cnf.isEmpty)
      return Some(literals)
    else if (cnf.exists(_.isEmpty))
      return None

    // unit propagation
    cnf.find(_.size == 1).map(_.head) match {
      case Some(unitLiteral) =>
        return dpll(simplify(cnf, literals + unitLiteral), literals + unitLiteral)
      case None =>
    }

    // pure propagation
    findPure(cnf) match {
      case Some(pureLiteral) =>
        return dpll(simplify(cnf, literals + pureLiteral), literals + pureLiteral)
      case None =>
    }

    // splitting
    val variable = extractVariables(cnf).head
    dpll(simplify(cnf, literals + variable), literals + variable) orElse dpll(simplify(cnf, literals + variable.neg), literals + variable.neg)
  }

  implicit class StreamUnfoldOps(stream: Stream.type) {
    // https://github.com/tpolecat/examples/blob/ab444af9101b9049d6bd7ebf13ae583bc77ac60a/src/main/scala/eg/Unfold.scala
    def unfold[A, B](a: A)(f: A => Option[(A, B)]): Stream[B] =
      f(a).map{ case (a, b) => b #:: unfold(a)(f)}.getOrElse(Stream.empty)

    def unfold0[A](a: A)(f: A => Option[A]): Stream[A] =
      unfold(a)(a => f(a).map(a => (a, a)))
  }

  def solveAll[A](satSolve: CNF[A] => Option[Set[Literal[A]]])(cnf: CNF[A]): List[Set[Literal[A]]] = {
    Stream.unfold(cnf) { cnf =>
      satSolve(cnf).map(solution => (solution.map(_.neg).toList :: cnf, solution))
    }.toList
  }

  implicit def toLiteral[A](variable: A): PosLiteral[A] = PosLiteral(variable)

  def main(args: Array[String]): Unit = {
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
