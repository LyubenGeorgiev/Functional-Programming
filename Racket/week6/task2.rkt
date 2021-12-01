#lang racket


(define (my-reverse-foldr xs)
  (foldr (λ (a b) (append b (list a))) '() xs)
  )


(equal? (my-reverse-foldr '(1 2 3 4 5)) '(5 4 3 2 1))