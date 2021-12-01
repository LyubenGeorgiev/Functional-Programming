#lang racket

(define (growing-plant upSpeed downSpeed desiredHeight)
  (define (helper up down desired res current)
    (if (>= (+ current up) desired)
      res
      (helper upSpeed downSpeed desiredHeight (add1 res) (- (+ current up) downSpeed))
      )
    )
  (helper upSpeed downSpeed desiredHeight 1 0)
  )

(= (growing-plant 5 2 5) 1)
(= (growing-plant 5 2 6) 2)
(= (growing-plant 10 9 4) 1)
(= (growing-plant 100 10 910) 10) ; upSpeed=100, downSpeed=10, desiredHeight=910