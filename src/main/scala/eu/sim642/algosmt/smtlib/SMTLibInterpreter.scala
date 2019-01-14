package eu.sim642.algosmt.smtlib

import eu.sim642.algosmt.core._
import eu.sim642.algosmt.logic.Logic
import eu.sim642.algosmt.logic.idl.{IntegerDifferenceLogic, IntegerDifferenceLogic2}
import eu.sim642.algosmt.logic.pl.PropositionalLogic
import eu.sim642.algosmt.smt.DPLLSMTSolver
import eu.sim642.algosmt.smt.cnf.CNFConverter

import scala.collection.mutable
import scala.io.{Source, StdIn}

trait SMTLibInterpreterLike {
  def execute(sexp: SExp): Seq[Either[String, SExp]]
}

class SMTLibInterpreter[A, B, C](logic: Logic[A, B, C]) extends SMTLibInterpreterLike {
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

  def execute(sexp: SExp): Seq[Either[String, SExp]] = sexp match {
    case Application("assert", exp) =>
      val bexp = parser.fromSExp(preprocess(exp))
      assertions += bexp
      Seq.empty

    case Application("check-sat") =>
      val bexp = assertions.reduce(And(_, _))
      val cnf = CNFConverter.convertFlat(bexp)
      val startTime = System.nanoTime()
      modelOption = solver.solve(cnf)
      val endTime = System.nanoTime()
      val duration = endTime - startTime
      Seq(
        Left(s"Solved in ${duration / 1000000000.0} s"),
        Right(Atom(modelOption.map(model => "sat").getOrElse("unsat")))
      )

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

  def execute(in: String): Seq[Either[String, SExp]] = {
    val in2 = in.takeWhile(_ != '#')
    if (in2.trim.isEmpty)
      Seq.empty
    else {
      SExpParser.parse(in2) match {
        case SExpParser.Success(result, next) => execute(result)
        case SExpParser.NoSuccess(msg, next) =>
          Seq(Left(s"Parse error $in2: $msg ($next)"))
      }
    }
  }
}

trait SetInfoStatus extends SMTLibInterpreterLike {
  private var expectedStatusOption: Option[SExp] = None

  abstract override def execute(sexp: SExp): Seq[Either[String, SExp]] = sexp match {
    case Application("set-info", Atom(":status"), status) =>
      expectedStatusOption = Some(status)
      Seq.empty

    case Application("check-sat") =>
      expectedStatusOption match {
        case Some(expectedStatus) =>
          val superRet = super.execute(sexp)
          if (superRet.exists(_.toOption == expectedStatusOption))
            superRet
          else
            superRet :+ Left(s"Expected status $expectedStatus")
        case None =>
          super.execute(sexp)
      }

    case _ => super.execute(sexp)
  }
}

object SMTLibInterpreter {
  def main(args: Array[String]): Unit = {
    //val smt = new SMTLibInterpreter(PropositionalLogic)
    //val smt = new SMTLibInterpreter(IntegerDifferenceLogic)
    val smt = new SMTLibInterpreter(IntegerDifferenceLogic2) with SetInfoStatus

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
