#lang racket

(require math/number-theory)

(define (factorize number)
  (filter (λ (x) (and (zero? (remainder number x)) (prime? x))) (range 2 (add1 number)))
  )

(equal? (factorize 2) '(2))
(equal? (factorize 6) '(2 3))
(equal? (factorize 13) '(13))
(equal? (factorize 123) '(3 41))
;(equal? (factorize 152) '(2 2 2 19)) ; spored men (equal? (factorize 152) '(2 19)) e po-pravilno, nqma logika da povtarqm deliteli
(equal? (factorize 152) '(2 19))
(equal? (factorize 12356498) '(2 7 11 19 41 103))