(set-info :status unsat)

; for Z3
(declare-const x1 Int)
(declare-const x2 Int)


(assert (distinct x1 x2))

(assert (<= 1 x1 2))
(assert (<= 1 x2 2))

(assert (distinct (- x1 x2) 1))
(assert (distinct (- x2 x1) 1))

(check-sat)
