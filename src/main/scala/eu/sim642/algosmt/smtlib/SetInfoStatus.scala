package eu.sim642.algosmt.smtlib

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
