#lang racket


(define (p n)
  (cond
    [(< n 1) (error "Expecting natural number!")]
    [else (/ (* n (sub1 (* 3 n))) 2)]
    )
  )


(= (p 1) 1)
(= (p 2) 5)
(= (p 3) 12)
(= (p 4) 22)
(= (p 5) 35)
(= (p 6) 51)