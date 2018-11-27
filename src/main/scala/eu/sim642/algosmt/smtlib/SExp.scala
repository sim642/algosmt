package eu.sim642.algosmt.smtlib

sealed trait SExp

case class Atom(str: String) extends SExp
case class Compound(sexps: SExp*) extends SExp
