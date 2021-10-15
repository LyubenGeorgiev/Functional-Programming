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

(define (count-digits-rec num)
  (cond
    [(< num 0) (error "n was negative")]
    [(zero? num) 0]
    [else (add1 (count-digits-rec (quotient num 10)))]
    )
  )

(= (count-digits-iter 12345) 5)
(= (count-digits-iter 123) 3)
; (count-digits-iter -13) ; error "n was negative"

(= (count-digits-rec 12345) 5)
(= (count-digits-rec 123) 3)
