package eu.sim642.algosmt.theory

import eu.sim642.algosmt.smtlib.{Application, Atom, Compound, SExp}

abstract class Theory {
  val leftAssocFuncs: Set[String]
  val chainableFuncs: Set[String]
  val pairwiseFuncs: Set[String]

  def preprocess(sexp: SExp): SExp = sexp match {
    case Application(func, args@_*) if leftAssocFuncs.contains(func) && args.length > 2 =>
      preprocess(Application(func, Application(func, args.init: _*), args.last))

    case Application(func, args@_*) if chainableFuncs.contains(func) && args.length > 2 =>
      preprocess(Application("and",
        args.zip(args.tail)
          .map({ case (arg1, arg2) => Application(func, arg1, arg2) }): _*
      ))

    case Application(func, args@_*) if pairwiseFuncs.contains(func) && args.length > 2 =>
      val argHead +: argTail = args
      preprocess(Application("and",
        argTail.map(arg => Application(func, argHead, arg)) :+
          Application(func, argTail: _*): _*
      ))

    case Atom(str) => Atom(str)
    case Compound(exps@_*) => Compound(exps.map(preprocess): _*)
  }

  def +(that: Theory): AggregateTheory = new AggregateTheory(this, that)
}
