(assert (distinct betty ethel joan kitty mary))

(assert (<= 1 betty 5))
(assert (<= 1 ethel 5))
(assert (<= 1 joan 5))
(assert (<= 1 kitty 5))
(assert (<= 1 mary 5))

(assert (xor (= kitty 2) (= betty 3)))
(assert (xor (= ethel 1) (= joan 2)))
(assert (xor (= joan 3) (= ethel 5)))
(assert (xor (= kitty 2) (= mary 4)))
(assert (xor (= mary 4) (= betty 1)))

(check-sat)
(get-model)
