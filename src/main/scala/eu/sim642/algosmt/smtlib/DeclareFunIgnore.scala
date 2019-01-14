package eu.sim642.algosmt.smtlib

trait DeclareFunIgnore extends SMTLibInterpreterLike {
  abstract override def execute(sexp: SExp): Seq[Either[String, SExp]] = sexp match {
    case Application("declare-fun", Atom(variable), Compound(argSorts@_*), Atom(retSort)) =>
      Seq.empty // ignore

    case Application("declare-const", Atom(variable), Atom(sort)) =>
      Seq.empty // ignore

    case _ => super.execute(sexp)
  }
}
