(set-info :status sat)

; for Z3
(declare-const baker Int)
(declare-const cooper Int)
(declare-const fletcher Int)
(declare-const miller Int)
(declare-const smith Int)


(assert (distinct baker cooper fletcher miller smith))

(assert (<= 1 baker 5))
(assert (<= 1 cooper 5))
(assert (<= 1 fletcher 5))
(assert (<= 1 miller 5))
(assert (<= 1 smith 5))

(assert (distinct baker 5))
(assert (distinct cooper 1))
(assert (and (distinct fletcher 1) (distinct fletcher 5)))
(assert (> miller cooper))
(assert (and (distinct smith (+ fletcher 1)) (distinct smith (+ fletcher (- 1)))))
(assert (and (distinct fletcher (+ cooper 1)) (distinct fletcher (+ cooper (- 1)))))

(check-sat)
(get-model)
