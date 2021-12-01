#lang racket


(define (num-to-xs x)
  (define (helper left-over)
    (if (< left-over 10)
        (list left-over)
        (append (helper (quotient left-over 10)) (list (remainder left-over 10)))
        )
    )
  (helper x)
  )

(define (get-distribution x)
  (remove-duplicates (map (λ (y) (cons y (length (filter (λ (z) (= z y)) (num-to-xs (* x x)))))) (sort (num-to-xs (* x x)) <)))
  )

(get-distribution 123)