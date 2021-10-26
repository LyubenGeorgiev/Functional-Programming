#lang racket

(define (g f y)
  (λ (x) (cond
           [(< (f x) y) x]
           [else (f x)]
           ))
  )

(= ((g (λ (x) (* 2 x)) 100) 50) 100)
(= ((g (λ (x) (* 2 x)) 100.236) 500.002) 1000.004)
(= ((g identity 1.001) 1.001) 1.001)