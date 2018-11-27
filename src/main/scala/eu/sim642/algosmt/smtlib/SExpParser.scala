package eu.sim642.algosmt.smtlib

import scala.util.parsing.combinator._

object SExpParser extends RegexParsers {
  def atom: Parser[Atom] = """[^\(\)\s]+""".r ^^ Atom

  def compound: Parser[Compound] = "(" ~> rep(sexp) <~ ")" ^^ (Compound(_: _*))

  def sexp: Parser[SExp] = (
    atom
  | compound
  )

  def parse(in: CharSequence): ParseResult[SExp] = parseAll(sexp, in)

  def parseMultiple(in: CharSequence): ParseResult[Seq[SExp]] = parseAll(rep(sexp), in)
}
