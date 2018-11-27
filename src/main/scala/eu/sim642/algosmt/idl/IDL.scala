package eu.sim642.algosmt.idl

import eu.sim642.algosmt.bool.Var
import eu.sim642.algosmt.idl.BellmanFord.Edge

object IDL {
  case class Constraint[A](x: A, y: A, n: Int) {
    override def toString: String = s"$x - $y ≤ $n"
  }

  def solve[A](variables: Seq[A], constraints: Seq[Constraint[A]]): Option[Map[A, Int]] = {
    sealed trait ConstraintVertex
    case object SourceVertex extends ConstraintVertex
    case class VariableVertex(x: A) extends ConstraintVertex

    val vertices: Seq[ConstraintVertex] = SourceVertex +: variables.map(VariableVertex)
    val edges: Seq[Edge[ConstraintVertex]] =
      constraints.map({ case Constraint(x, y, n) => (VariableVertex(y), n, VariableVertex(x)) }) ++
      variables.map(x => (SourceVertex, 0, VariableVertex(x)))

    BellmanFord.bellmanFord(vertices, edges, SourceVertex).map({ distance =>
      distance.flatMap({
        case (SourceVertex, _) => None
        case (VariableVertex(x), value) => Some(x -> value)
      })
    })
  }

  def parseConstraints(in: CharSequence): Seq[Constraint[String]] = {
    IDLConstraintBExpParser.parseMultiple(in).map({
      case Var(constraint) => constraint
    })
  }

  def extractVariables[A](constraints: Seq[Constraint[A]]): Seq[A] = {
    constraints.flatMap({ case Constraint(x, y, n) => Seq(x, y) }).distinct // TODO: don't use distinct
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
