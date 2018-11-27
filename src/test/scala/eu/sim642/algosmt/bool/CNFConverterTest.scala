package eu.sim642.algosmt.bool

import org.scalatest.FunSuite

class CNFConverterTest extends FunSuite {
  import CNFConverter.convert

  test("Converts double negation") {
    assert(convert(Not(Not(Var("x")))) == Var("x"))
  }

  test("Converts De Morgan") {
    assert(convert(Not(And(Var("x"), Var("y")))) == Or(Not(Var("x")), Not(Var("y"))))
    assert(convert(Not(Or(Var("x"), Var("y")))) == And(Not(Var("x")), Not(Var("y"))))
  }

  test("Propagates NNF") {
    assert(convert(Not(Not(Not(Not(Var("x")))))) == Var("x"))
    assert(convert(Not(Or(Not(Not(Var("x"))), Not(Not(Var("y")))))) == And(Not(Var("x")), Not(Var("y"))))
    assert(convert(Not(And(Not(Not(Var("x"))), Not(Not(Var("y")))))) == Or(Not(Var("x")), Not(Var("y"))))

    assert(convert(And(Not(Not(Var("x"))), Not(Not(Var("y"))))) == And(Var("x"), Var("y")))
    assert(convert(Or(Not(Not(Var("x"))), Not(Not(Var("y"))))) == Or(Var("x"), Var("y")))
  }

  test("Distributes") {
    assert(convert(Or(And(Var("x"), Var("y")), Var("z"))) == And(Or(Var("x"), Var("z")), Or(Var("y"), Var("z"))))
    assert(convert(Or(Var("x"), And(Var("y"), Var("z")))) == And(Or(Var("x"), Var("y")), Or(Var("x"), Var("z"))))

    // assert(convert(Or(And(Var("x"), Var("y")), And(Var("z"), Var("w")))) == ???) // tree structure not guaranteed
  }
}
