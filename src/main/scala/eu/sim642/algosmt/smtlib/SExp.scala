package eu.sim642.algosmt.smtlib

sealed trait SExp

case class Atom(str: String) extends SExp {
  override def toString: String = str
}

case class Compound(sexps: SExp*) extends SExp {
  override def toString: String = sexps.mkString("(", " ", ")")
}

object Application {
  def apply(func: String, args: SExp*): SExp = Compound(Atom(func) +: args: _*)

  def unapplySeq(sexp: SExp): Option[(String, Seq[SExp])] = sexp match {
    case Compound(Atom(func), args@_*) => Some((func, args))
    case _ => None
  }
}
