(set-info :status sat)

(assert (distinct x1 x2 x3 x4))

(assert (<= 1 x1 4))
(assert (<= 1 x2 4))
(assert (<= 1 x3 4))
(assert (<= 1 x4 4))

(assert (distinct (- x1 x2) 1))
(assert (distinct (- x2 x1) 1))

(assert (distinct (- x2 x3) 1))
(assert (distinct (- x3 x2) 1))

(assert (distinct (- x3 x4) 1))
(assert (distinct (- x4 x3) 1))

(assert (distinct (- x1 x3) 2))
(assert (distinct (- x3 x1) 2))

(assert (distinct (- x2 x4) 2))
(assert (distinct (- x4 x2) 2))

(assert (distinct (- x1 x4) 3))
(assert (distinct (- x4 x1) 3))

(check-sat)
(get-model)
