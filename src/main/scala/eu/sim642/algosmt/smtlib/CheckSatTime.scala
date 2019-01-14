package eu.sim642.algosmt.smtlib

import eu.sim642.algosmt.util.TimeUtil

trait CheckSatTime extends SMTLibInterpreterLike {
  abstract override def execute(sexp: SExp): Seq[Either[String, SExp]] = sexp match {
    case Application("check-sat") =>
      val (superRet, duration) = TimeUtil.measureTime(super.execute(sexp)) // includes CNF conversion
      Left(s"check-sat in $duration s") +: superRet

    case _ => super.execute(sexp)
  }
}
