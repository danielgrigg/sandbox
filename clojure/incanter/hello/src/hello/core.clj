(ns hello.core)

(use '(incanter core stats charts))

(view (histogram (sample-normal 1000)))

(view (function-plot sin -10 10))

; 3x3 matrix
(def A (matrix [[1 2 3] [4 5 6] [7 8 9]]))

                                        ; 3x3 from a 1x9
(def A2 (matrix [1 2 3 4 5 6 7 8 9] 3 ))

                                        ; column vector

(def B (matrix [1 2 3 4 5 6 7 8 9]))

; 3x4 initd with zeroes
(matrix 0 3 4)

                                        ; identity
(identity-matrix 4)

; guess!
(diag [1 2 3 4])

                                        ; extract diagonal
(diag A)

(symmetric-matrix
 [1
  2 3
  4 5 6
  7 8 9 10])

                                        ; matrix ops
; plus minus mult div abs exp sqrt pow cos acos sin asin tan atan sum prod.

(plus A A2)
(plus 3 A)
(mmult A A2)

(decomp-cholesky)

                                        ; inverse of matrix...i want to wrap this
(solve (identity-matrix 4))

; supports ISeq ops
(map sum A)
(reduce plus A)
(sum (first A))
(second (first A))
(filter #(> (nth % 1) 4) A)

;[1.0000 2.0000 3.0000
;4.0000 5.0000 6.0000
;7.0000 8.0000 9.0000]

