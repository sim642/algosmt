package eu.sim642.algosmt.smtlib

import eu.sim642.algosmt.core._
import eu.sim642.algosmt.logic.Logic
import eu.sim642.algosmt.logic.idl.IntegerDifferenceLogic
import eu.sim642.algosmt.logic.pl.PropositionalLogic
import eu.sim642.algosmt.smt.DPLLSMTSolver

import scala.collection.mutable
import scala.io.StdIn

class SMTLibInterpreter[A, B, C](logic: Logic[A, B, C]) {
  private val theory = logic.theory
  private val parser = logic.parser
  private val solver = new DPLLSMTSolver(logic.solver)

  private val assertions: mutable.Buffer[BExp[A]] = mutable.Buffer.empty
  private var modelOption: Option[Map[B, C]] = None

  private def preprocess(sexp: SExp): SExp = sexp match {
    case Application(func, args@_*) if theory.leftAssocFuncs.contains(func) && args.length > 2 =>
      preprocess(Application(func, Application(func, args.init: _*), args.last))

    case Application(func, args@_*) if theory.chainableFuncs.contains(func) && args.length > 2 =>
      preprocess(Application("and",
        args.zip(args.tail)
          .map({ case (arg1, arg2) => Application(func, arg1, arg2) }): _*
      ))

    case Application(func, args@_*) if theory.pairwiseFuncs.contains(func) && args.length > 2 =>
      val argHead +: argTail = args
      preprocess(Application("and",
        argTail.map(arg => Application(func, argHead, arg)) :+
          Application(func, argTail: _*): _*
      ))

    case Atom(str) => Atom(str)
    case Compound(exps@_*) => Compound(exps.map(preprocess): _*)
  }

  def execute(sexp: SExp): Either[String, Option[SExp]] = sexp match {
    case Application("assert", exp) =>
      val bexp = parser.fromSExp(preprocess(exp))
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
          Right(Some(Compound(model.map({ case (variable, value) => logic.toSExp(variable, value) }).toSeq: _*)))

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
    //val smt = new SMTLibInterpreter(PropositionalLogic)
    val smt = new SMTLibInterpreter(IntegerDifferenceLogic)

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
