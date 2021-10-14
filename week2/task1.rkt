#lang racket

(define (count-digits-iter num)
  (define (helper curNum res)
    (cond
      [(zero? curNum) res]
      [else (helper (quotient curNum 10) (+ res 1))]
      )
    )
  (cond
    [(< num 0) (error "n was negative")]
    [else (helper (quotient num 10) 1)]
    )
  
  )

(= (count-digits-iter 12345) 5)
(= (count-digits-iter 123) 3)
; (count-digits-iter -13) ; error "n was negative"