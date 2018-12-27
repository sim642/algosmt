# algosmt
SMT solver project for Advanced Algorithmics course at University of Tartu.

## Team
* Simmo Saan
* ???

## Satisfiability Modulo Integer Difference Logic
### Description
Boolean satisfiability (SAT) problems, which consist of purely boolean algebra expressions, can be solved by SAT solvers. Satisfiability modulo theories (SMT) problems are a generalization of SAT, where the logical expressions may, besides boolean variables, contain for example integer variables and conditions between them. Such problems are solved by SMT solvers.
	
Integer difference logic (IDL) in particular is a very small subset of integer conditions that are allowed to be used. In IDL, all the conditions have the form x-y≤n, where x, y are integer variables and n an integer constant. This very limited integer logic also has very simple decision procedure, yet is expressive enough for many problems.

### Motivation
While many complicated problems can be described as SAT problems, which can be given to SAT solvers to solve, describing the problem purely with booleans and constructing the respective conjunctive normal form (CNF) can be difficult. Being able to use integer variables and integer conditions, makes modeling of many problems (e.g. in scheduling) as SMT problems much easier and intuitive.

### Main challenge
The biggest challenge and goal is to understand is to understand how SAT solving and IDL solving is interleaved to get a combined SMT solver.

