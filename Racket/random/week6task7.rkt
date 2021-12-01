#lang racket


(define (ordered xs)
  (λ (pred) (foldl (λ (index result) (and result (pred (car (drop xs index)) (car (drop xs (add1 index)))))) #t (range (sub1 (length xs)))))
  )


; with freedom to solve however you like it
(equal? ((ordered '(1 2 3 5)) (λ (x y) (< x y))) #t)
(equal? ((ordered '(1 8 29 92)) (λ (x y) (= y (+ (* x 3) 5)))) #t)
(equal? ((ordered '(1 8 3 14)) (λ (x y) (= y (+ (* x 3) 5)))) #f)