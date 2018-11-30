package eu.sim642.algosmt.smtlib

import eu.sim642.algosmt.bool.{And, BExp, BExpParser}

trait PairwiseParser[A] extends BExpParser[A] {
  def pairwiseFuncs: Set[String]

  abstract override def fromSExp(sexp: SExp): BExp[A] = sexp match {
    case Application(func, args@_*) if pairwiseFuncs.contains(func) && args.length > 2 =>
      val argHead +: argTail = args
      And(argTail
        .map(arg => fromSExp(Application(func, argHead, arg)))
        .reduce(And(_, _)), // TODO: use and left-assoc
        fromSExp(Application(func, argTail: _*))
      )

    case exp => super.fromSExp(exp)
  }
}
