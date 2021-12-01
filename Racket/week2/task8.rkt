#lang racket

(define (interesting? num)
  (define (sum-of-digits n)
    (cond
      [(zero? n) 0]
      [else (+ (remainder n 10) (sum-of-digits (quotient n 10)))]
      )
    )
  (zero? (remainder num (sum-of-digits num)))
  )

(equal? (interesting? 410) #t)