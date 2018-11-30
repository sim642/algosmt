package eu.sim642.algosmt.core

sealed trait BExp[A]

case class Var[A](name: A) extends BExp[A]
case class Not[A](exp: BExp[A]) extends BExp[A]
case class And[A](left: BExp[A], right: BExp[A]) extends BExp[A]
case class Or[A](left: BExp[A], right: BExp[A]) extends BExp[A]
