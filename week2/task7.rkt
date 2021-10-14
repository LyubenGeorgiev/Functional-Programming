#lang racket

(define (count-occurences num digit)
  (cond
    [(< num 0) (error "Negative number!")]
    [(zero? num) 0]
    [(= (remainder num 10) digit) (add1 (count-occurences (quotient num 10) digit))]
    [else (count-occurences (quotient num 10) digit)]
    )
  )

(= (count-occurences 121 1) 2)
; (count-occurences -121 1) ; error "Negative number!"