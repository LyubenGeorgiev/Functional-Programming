#lang racket


(define (num-bigger-elements xs)
  (map (λ (x) (cons x (list (length (filter (λ (y) (> y x)) xs))))) xs)
  )


(num-bigger-elements '(5 6 3 4)); → '((5 1) (6 0) (3 3) (4 2))
(num-bigger-elements '(1 1 1)); → '((1 0) (1 0) (1 0))