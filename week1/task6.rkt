#lang racket

(define (rev num)
  (define (helper current result)
    (if (zero? current)
      result
      (helper (quotient current 10) (+ (* result 10) (remainder current 10)))
      )
    )
  (helper (quotient num 10) (remainder num 10))
  )

(= (rev 0) 0)
(= (rev 1) 1)
(= (rev 123) 321)
(= (rev 987654321) 123456789)