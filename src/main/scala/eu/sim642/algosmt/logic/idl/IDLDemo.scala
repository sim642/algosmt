package eu.sim642.algosmt.logic.idl

import eu.sim642.algosmt.core.Var

object IDLDemo extends IDLSolver[String] {

  def parseConstraints(in: CharSequence): Seq[Constraint[String]] = {
    IDLParser.parseMultiple(in).map({
      case Var(constraint) => constraint
    })
  }

  def main(args: Array[String]): Unit = {
    /*println(solve(Seq("x1", "x2", "x3"), Seq(
      Constraint("x1", "x2", 2),
      Constraint("x2", "x3", 1),
      Constraint("x3", "x1", -1),
    )))

    println(solve(Seq("x1", "x2", "x3"), Seq(
      Constraint("x1", "x2", 2),
      Constraint("x2", "x3", 1),
      Constraint("x3", "x1", -4),
    )))*/

    val constr1 = parseConstraints(
      """
        |(<= (- x1 x2) 2)
        |(<= (- x2 x3) 1)
        |(<= (- x3 x1) (- 1))
      """.stripMargin)
    println(solve(extractVariables(constr1), constr1))

    val constr2 = parseConstraints(
      """
        |(<= (- x1 x2) 2)
        |(<= (- x2 x3) 1)
        |(<= (- x3 x1) (- 4))
      """.stripMargin)
    println(solve(extractVariables(constr2), constr2))
  }
}
