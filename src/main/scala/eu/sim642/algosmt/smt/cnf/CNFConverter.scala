package eu.sim642.algosmt.smt.cnf

import eu.sim642.algosmt.core._

object CNFConverter {
  def convert[A](exp: BExp[A]): BExp[A] = distribute(convertNNF(exp))

  private def convertNNF[A](exp: BExp[A]): BExp[A] = exp match {
    case Not(Not(exp)) => convertNNF(exp)
    case Not(And(left, right)) => Or(convertNNF(Not(left)), convertNNF(Not(right)))
    case Not(Or(left, right)) => And(convertNNF(Not(left)), convertNNF(Not(right)))
    case And(left, right) => And(convertNNF(left), convertNNF(right))
    case Or(left, right) => Or(convertNNF(left), convertNNF(right))

    case exp => exp
  }

  private def distributeOr[A](left: BExp[A], right: BExp[A]): BExp[A] = (left, right) match {
    case (And(leftleft, leftright), right) => And(distributeOr(leftleft, right), distributeOr(leftright, right))
    case (left, And(rightleft, rightright)) => And(distributeOr(left, rightleft), distributeOr(left, rightright))
    case (left, right) => Or(left, right)
  }

  private def distribute[A](exp: BExp[A]): BExp[A] = exp match {
    case And(left, right) => And(distribute(left), distribute(right))
    case Or(left, right) => distributeOr(distribute(left), distribute(right))

    case exp => exp
  }


  def convertFlat[A](exp: BExp[A]): CNF[A] = flattenCNF(convert(exp))

  private def flattenDisjunct[A](exp: BExp[A]): Disjunct[A] = exp match {
    case Var(name) => Set(PosLiteral(name))
    case Not(Var(name)) => Set(NegLiteral(name))
    case Or(left, right) => flattenDisjunct(left) ++ flattenDisjunct(right)
  }

  private def flattenCNF[A](exp: BExp[A]): CNF[A] = exp match {
    case And(left, right) => flattenCNF(left) ++ flattenCNF(right)
    case exp => Set(flattenDisjunct(exp))
  }
}
