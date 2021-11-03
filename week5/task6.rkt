#lang racket

(require math/number-theory)

(define (factorize number)
  (define (helper current num)
    (cond
      [(> current num) '()]
      [else (cond
              [(and (zero? (remainder num current)) (prime? current)) (cons current (helper current (quotient num current)))]
              [else (helper (add1 current) num)]
              )]
      )
    )
  (helper 2 number)
  )

(equal? (factorize 2) '(2))
(equal? (factorize 6) '(2 3))
(equal? (factorize 13) '(13))
(equal? (factorize 123) '(3 41))
(equal? (factorize 152) '(2 2 2 19))
(equal? (factorize 12356498) '(2 7 11 19 41 103))