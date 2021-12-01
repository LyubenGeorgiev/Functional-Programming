#lang racket

(define (max-multiple divisor bound)
  (define (helper current)
    (cond
      [(zero? current) (error "Invalid input!")]
      [(zero? (remainder current divisor)) current]
      [else (helper (sub1 current))]
      )
    )
  (helper bound)
  )

(= (max-multiple 2 7) 6)
(= (max-multiple 3 10) 9)
(= (max-multiple 7 17) 14)
(= (max-multiple 10 50) 50)
(= (max-multiple 37 200) 185)
(= (max-multiple 7 100) 98)