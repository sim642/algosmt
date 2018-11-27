package eu.sim642.algosmt.smtlib

sealed trait SExp

case class Atom(str: String) extends SExp
case class Compound(sexps: SExp*) extends SExp

object Application {
  def apply(func: String, args: SExp*): SExp = Compound(Atom(func) +: args: _*)

  def unapplySeq(sexp: SExp): Option[(String, Seq[SExp])] = sexp match {
    case Compound(Atom(func), args@_*) => Some((func, args))
    case _ => None
  }
}
