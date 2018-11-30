package eu.sim642.algosmt.smtlib

import eu.sim642.algosmt.bool.{And, BExp, BExpParser, CNFConverter}
import eu.sim642.algosmt.smt.SMTSolver

import scala.collection.mutable
import scala.io.StdIn

class SMTLibInterpreter[A, B, C](private val parser: BExpParser[A], private val solver: SMTSolver[A, B, C]) {
  private val assertions: mutable.Buffer[BExp[A]] = mutable.Buffer.empty
  private var modelOption: Option[Map[B, C]] = None

  def execute(sexp: SExp): Either[String, Option[SExp]] = sexp match {
    case Application("assert", exp) =>
      val bexp = parser.fromSExp(exp)
      assertions += bexp
      Right(None)

    case Application("check-sat") =>
      val bexp = assertions.reduce(And(_, _))
      val cnf = CNFConverter.convertFlat(bexp)
      modelOption = solver.solve(cnf)
      Right(Some(Atom(modelOption.map(model => "sat").getOrElse("unsat"))))

    case Application("get-model") =>
      modelOption match {
        case Some(model) =>
          // TODO: non-standard get-model, more like get-value for all variables
          Right(Some(Compound(model.map({ case (variable, value) => Compound(Atom(variable.toString), Atom(value.toString)) }).toSeq: _*)))

        case None =>
          Left("Model error")
      }

    case exp =>
      Left(s"Match error: $exp")
  }

  def execute(in: CharSequence): Either[String, Option[SExp]] = {
    SExpParser.parse(in) match {
      case SExpParser.Success(result, next) => execute(result)
      case SExpParser.NoSuccess(msg, next) =>
        Left(s"Parse error: $msg")
    }
  }
}

object SMTLibInterpreter {
  def main(args: Array[String]): Unit = {
    //val smt = new SMTLibInterpreter(BExpParser.pureBooleanParser, SMTSolver.pureDpllSolver)
    val smt = new SMTLibInterpreter(BExpParser.idlBooleanParser, SMTSolver.idlDpllSolver)

    var line = StdIn.readLine()
    while (line != null) {
      smt.execute(line) match {
        case Left(error) => Console.err.println(error)
        case Right(None) =>
        case Right(Some(sexp)) => println(sexp)
      }
      line = StdIn.readLine()
    }
  }
}
