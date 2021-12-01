#lang racket

(define (snail desiredHeight upSpeed downSpeed)
  (define (helper up down desired res current)
    (if (>= (+ current up) desired)
      res
      (helper upSpeed downSpeed desiredHeight (add1 res) (- (+ current up) downSpeed))
      )
    )
  (helper upSpeed downSpeed desiredHeight 1 0)
  )

(= (snail 3 2 1) 2)
(= (snail 10 3 1) 5)
(= (snail 10 3 2) 8)
(= (snail 100 20 5) 7)
(= (snail 5 10 3) 1)