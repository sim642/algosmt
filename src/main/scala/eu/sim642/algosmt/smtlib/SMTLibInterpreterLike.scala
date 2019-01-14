package eu.sim642.algosmt.smtlib

trait SMTLibInterpreterLike {
  def execute(sexp: SExp): Seq[Either[String, SExp]]
}
