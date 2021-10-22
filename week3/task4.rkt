#lang racket


(define (sum-of-digits x)
  (cond
    [(zero? x) 0]
    [else (+ (remainder x 10) (sum-of-digits (quotient x 10)))]
    )
  )

(define (sum-divisible-numbers start finish k)
  (define (sum-divisible-numbers-helper current)
    (cond
      [(> current finish) 0]
      [(zero? (remainder (sum-of-digits current) k)) (+ current (sum-divisible-numbers-helper (add1 current)))]
      [else (sum-divisible-numbers-helper (add1 current))]
      )
    )
  (cond
    [(< finish start) (sum-divisible-numbers finish start k)]
    [else (sum-divisible-numbers-helper start)]
    )
  )

(= (sum-divisible-numbers 0 10 5) 5)
(= (sum-divisible-numbers 0 100 5) 990)
(= (sum-divisible-numbers 100 0 5) 990)