package eu.sim642.algosmt.core

import eu.sim642.algosmt.logic.idl.IntegerDifferenceLogic
import eu.sim642.algosmt.logic.pl.PropositionalLogic
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

object BExpParser {
  def main(args: Array[String]): Unit = {
    println(PropositionalLogic.parser.parse("(and p q)"))
    println(IntegerDifferenceLogic.parser.parse("(and (<= (- x1 x2) 2) (<= (- x2 x3) (- 1)))"))
  }
}
