(set-info :status unsat)

; for Z3
(declare-const x1 Int)
(declare-const x2 Int)
(declare-const x3 Int)


(assert (distinct x1 x2 x3))

(assert (<= 1 x1 3))
(assert (<= 1 x2 3))
(assert (<= 1 x3 3))

(assert (distinct (- x1 x2) 1))
(assert (distinct (- x2 x1) 1))

(assert (distinct (- x2 x3) 1))
(assert (distinct (- x3 x2) 1))

(assert (distinct (- x1 x3) 2))
(assert (distinct (- x3 x1) 2))

(check-sat)
