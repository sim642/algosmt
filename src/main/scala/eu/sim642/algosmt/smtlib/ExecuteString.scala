package eu.sim642.algosmt.smtlib

trait ExecuteString extends SMTLibInterpreterLike {
  def execute(in: String): Seq[Either[String, SExp]] = {
    val inTrimmed = in.takeWhile(_ != '#').trim
    if (inTrimmed.isEmpty)
      Seq.empty
    else {
      SExpParser.parse(inTrimmed) match {
        case SExpParser.Success(result, next) => execute(result)
        case SExpParser.NoSuccess(msg, next) =>
          Seq(Left(s"Parse error $inTrimmed: $msg ($next)"))
      }
    }
  }
}
