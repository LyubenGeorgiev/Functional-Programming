#lang racket

(define (sum-digits-iter num)
  (define (helper curNum res)
    (cond
      [(zero? curNum) res]
      [else (helper (quotient curNum 10) (+ res (remainder curNum 10)))]
      )
    )
  (cond
    [(< num 0) (error "n was negative")]
    [else (helper (quotient num 10) (remainder num 10))]
    )
  )

(= (sum-digits-iter 12345) 15)
(= (sum-digits-iter 123) 6)
; (sum-digits-iter -13) ; error "n was negative"
