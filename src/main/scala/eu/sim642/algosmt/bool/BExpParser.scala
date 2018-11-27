package eu.sim642.algosmt.bool

import eu.sim642.algosmt.idl.IDL
import eu.sim642.algosmt.idl.IDL.Constraint
import eu.sim642.algosmt.smtlib._

abstract class BExpParser[A] {

  def varFromSExp(sexp: SExp): BExp[A]

  def fromSExp(sexp: SExp): BExp[A] = sexp match {
    case Application("not", exp) => Not(fromSExp(exp))
    case Application("and", left, right) => And(fromSExp(left), fromSExp(right))
    case Application("or", left, right) => Or(fromSExp(left), fromSExp(right))
    case exp => varFromSExp(exp)
  }

  def parse(in: CharSequence): BExp[A] = {
    SExpParser.parse(in) match {
      case SExpParser.Success(result, next) => fromSExp(result)
      case SExpParser.NoSuccess(msg, next) => throw new RuntimeException(msg)
    }
  }
}

object StringBExpParser extends BExpParser[String] {
  override def varFromSExp(sexp: SExp): BExp[String] = sexp match {
    case Atom(str) => Var(str)
  }
}

object IDLBExpParser extends BExpParser[Constraint[String]] {
  override def varFromSExp(sexp: SExp): BExp[Constraint[String]] = Var(IDL.fromSExp(sexp))
}

object BExpParser {
  def main(args: Array[String]): Unit = {
    println(StringBExpParser.parse("(and p q)"))
    println(IDLBExpParser.parse("(and (<= (- x1 x2) 2) (<= (- x2 x3) 1))"))
  }
}
