package eu.sim642.algosmt.smtlib

import eu.sim642.algosmt.bool.{And, BExp, BExpParser}

trait ChainableParser[A] extends BExpParser[A] {
  def chainableFuncs: Set[String]

  abstract override def fromSExp(sexp: SExp): BExp[A] = sexp match {
    case Application(func, args@_*) if chainableFuncs.contains(func) && args.length > 2 =>
      args.zip(args.tail)
        .map({ case (exp1, exp2) => fromSExp(Application(func, exp1, exp2)) })
        .reduce(And(_, _)) // TODO: use and left-assoc

    case exp => super.fromSExp(exp)
  }
}
