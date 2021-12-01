#lang racket


(define (get-missing-length xss)
  (if (or (null? xss) (ormap null? xss))
    (error "Empty list!")
    (let ([lengths (sort (map length xss) <)])
      (findf number? (map (λ (x y) (if (= x y) #t x)) (range (car lengths) (+ (car lengths) (length xss))) lengths))
      )
    )
  )


(get-missing-length '((1 2) (4 5 1 1) (1) (5 6 7 8 9))); 3
(get-missing-length '(("a", "a", "a") ("a", "a") ("a", "a", "a",
"a") ("a") ("a", "a", "a", "a", "a", "a")));5