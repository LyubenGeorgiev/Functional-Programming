#lang racket


(define (where xs preds)
  (if (null? preds)
    xs
    (where (filter (car preds) xs) (cdr preds))
    )
  )

(equal? (where '(3 4 5 6 7 8 9 10) (list even? (lambda (x) (> x 5)))) '(6 8 10)) ; all even numbers greater than 5
(equal? (where '(3 4 5 7) (list even? (lambda (x) (> x 5)))) '()) ; no numbers are even and greater than 5