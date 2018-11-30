package eu.sim642.algosmt.smtlib

import eu.sim642.algosmt.bool.{And, BExp, BExpParser, CNFConverter}
import eu.sim642.algosmt.smt.SMTSolver

import scala.collection.mutable
import scala.io.StdIn

class SMTLibInterpreter {
  private val assertions: mutable.Buffer[BExp[String]] = mutable.Buffer.empty
  private var modelOption: Option[Map[String, Boolean]] = None

  def execute(sexp: SExp): Option[SExp] = sexp match {
    case Application("assert", exp) =>
      val bexp = BExpParser.pureBooleanParser.fromSExp(exp)
      assertions += bexp
      None

    case Application("check-sat") =>
      val bexp = assertions.reduce(And(_, _))
      val cnf = CNFConverter.convertFlat(bexp)
      modelOption = SMTSolver.pureDpllSolver.solve(cnf)
      Some(Atom(modelOption.map(model => "sat").getOrElse("unsat")))

    case Application("get-model") =>
      modelOption match {
        case Some(model) =>
          // TODO: non-standard get-model, more like get-value for all variables
          Some(Compound(model.map({ case (variable, value) => Compound(Atom(variable), Atom(value.toString)) }).toSeq: _*))

        case None =>
          println("Model error")
          None
      }

    case exp =>
      println(s"Match error: $exp")
      None
  }

  def execute(in: CharSequence): Option[SExp] = {
    SExpParser.parse(in) match {
      case SExpParser.Success(result, next) => execute(result)
      case SExpParser.NoSuccess(msg, next) =>
        println(s"Parse error: $msg")
        None
    }
  }
}

object SMTLibInterpreter {
  def main(args: Array[String]): Unit = {
    val smt = new SMTLibInterpreter

    var line = StdIn.readLine()
    while (line != null) {
      val outOption = smt.execute(line)
      if (outOption.isDefined)
        println(outOption.get)
      line = StdIn.readLine()
    }
  }
}
