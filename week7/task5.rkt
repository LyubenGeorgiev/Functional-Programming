#lang racket


(define (triangular? xss)
  (foldl (λ (t r) (and r t)) #t (map (λ (i) (foldl (λ (x res) (and res (zero? x))) #t (take (car (drop xss i)) i))) (range (length xss))))
  )

(equal? (triangular? '((1 2 3) (0 5 6) (0 0 9))) #t)
(equal? (triangular? '((0 2 3) (0 0 6) (1 0 0))) #f)
(equal? (triangular? '((1 2 3) (1 5 6) (0 0 9))) #f)
(equal? (triangular? '((1 2 3 4) (0 5 6 7) (0 0 8 9) (0 0 0 9))) #t)