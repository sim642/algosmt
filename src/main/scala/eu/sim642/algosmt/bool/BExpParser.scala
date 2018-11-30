package eu.sim642.algosmt.bool

import eu.sim642.algosmt.logic.idl.IDLParser
import eu.sim642.algosmt.logic.pl.VarParser
import eu.sim642.algosmt.smtlib._

trait BExpParser[A] {
  def fromSExp(sexp: SExp): BExp[A]

  def parse(in: CharSequence): BExp[A] = {
    SExpParser.parse(in) match {
      case SExpParser.Success(result, next) => fromSExp(result)
      case SExpParser.NoSuccess(msg, next) => throw new RuntimeException(msg)
    }
  }

  def parseMultiple(in: CharSequence): Seq[BExp[A]] = {
    SExpParser.parseMultiple(in) match {
      case SExpParser.Success(result, next) => result.map(fromSExp)
      case SExpParser.NoSuccess(msg, next) => throw new RuntimeException(msg)
    }
  }
}

case class BooleanBExpParser[A](delegate: BExpParser[A]) extends BExpParser[A] {
  def fromSExp(sexp: SExp): BExp[A] = sexp match {
    case Application("not", exp) => Not(fromSExp(exp))
    case Application("and", left, right) => And(fromSExp(left), fromSExp(right))
    case Application("or", left, right) => Or(fromSExp(left), fromSExp(right))
    case exp => delegate.fromSExp(exp)
  }
}

object BExpParser {
  val pureBooleanParser = BooleanBExpParser(VarParser)
  val idlBooleanParser = BooleanBExpParser(IDLParser)

  def main(args: Array[String]): Unit = {
    println(pureBooleanParser.parse("(and p q)"))
    println(idlBooleanParser.parse("(and (<= (- x1 x2) 2) (<= (- x2 x3) (- 1)))"))
  }
}
