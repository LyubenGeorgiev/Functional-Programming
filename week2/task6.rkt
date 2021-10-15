#lang racket

(require math/number-theory)

(define (sum-special-primes maxCount contains)
  (define (contains-helper num)
    (cond
      [(zero? num) #f]
      [(= contains (remainder num 10)) #t]
      [else (contains-helper (quotient num 10))]
      )
    )
  (define (helper i curCount res)
    (cond
      [(= curCount maxCount) res]
      [(and (prime? i) (contains-helper i)) (helper (add1 i) (add1 curCount) (+ res i))]
      [else (helper (add1 i) curCount res)]
      )
    )
  (helper 2 0 0)
  )

(= (sum-special-primes 5 2) 392)
(= (sum-special-primes 5 3) 107)
(= (sum-special-primes 10 3) 462)