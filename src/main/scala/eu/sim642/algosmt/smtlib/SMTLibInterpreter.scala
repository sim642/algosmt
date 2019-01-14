package eu.sim642.algosmt.smtlib

import eu.sim642.algosmt.core._
import eu.sim642.algosmt.logic.Logic
import eu.sim642.algosmt.logic.idl.{IntegerDifferenceLogic, IntegerDifferenceLogic2}
import eu.sim642.algosmt.logic.pl.PropositionalLogic
import eu.sim642.algosmt.smt.DPLLSMTSolver
import eu.sim642.algosmt.smt.cnf.CNFConverter

import scala.collection.mutable
import scala.io.{Source, StdIn}


class SMTLibInterpreter[A, B, C](logic: Logic[A, B, C]) extends SMTLibInterpreterLike {
  private val theory = logic.theory
  private val parser = logic.parser
  private val solver = new DPLLSMTSolver(logic.solver)

  private val assertions: mutable.Buffer[BExp[A]] = mutable.Buffer.empty
  private var modelOption: Option[Map[B, C]] = None

  def execute(sexp: SExp): Seq[Either[String, SExp]] = sexp match {
    case Application("assert", exp) =>
      val bexp = parser.fromSExp(theory.preprocess(exp))
      assertions += bexp
      Seq.empty

    case Application("check-sat") =>
      val bexp = assertions.reduce(And(_, _))
      val cnf = CNFConverter.convertFlat(bexp)
      modelOption = solver.solve(cnf)
      Seq(Right(Atom(modelOption.map(model => "sat").getOrElse("unsat"))))

    case Application("get-model") =>
      modelOption match {
        case Some(model) =>
          // TODO: non-standard get-model, more like get-value for all variables
          Seq(Right(Compound(model.map({ case (variable, value) => logic.toSExp(variable, value) }).toSeq: _*)))

        case None =>
          Seq(Left("Model error"))
      }

    case exp =>
      Seq(Left(s"Match error: $exp"))
  }
}


object SMTLibInterpreter {
  def main(args: Array[String]): Unit = {
    //val smt = new SMTLibInterpreter(PropositionalLogic)
    //val smt = new SMTLibInterpreter(IntegerDifferenceLogic)
    val smt = new SMTLibInterpreter(IntegerDifferenceLogic2) with CheckSatTime with SetInfoStatus with DeclareFunIgnore with ExecuteString

    val source = args match {
      case Array() => Source.stdin
      case Array(fileName) => Source.fromFile(fileName)
    }

    for (line <- source.getLines()) {
      smt.execute(line).foreach({
        case Left(error) => Console.err.println(error)
        case Right(sexp) => println(sexp)
      })
    }
  }
}
