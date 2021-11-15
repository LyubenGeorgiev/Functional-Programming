#lang racket

(require math/number-theory)


(define (numbers n)
  (λ (k) (filter (λ (x) (< (length (filter (λ (y) (and (divides? y x) (prime? y))) (range 2 x))) k)) (range 1 (add1 n))))
  )


(equal? ((numbers 10) 1) '(1 2 3 5 7))
(equal? ((numbers 20) 2) '(1 2 3 4 5 7 8 9 11 13 16 17 19))