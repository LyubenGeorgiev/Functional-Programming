#lang racket

(define (amicable? a b)
  (define (divisors-sum num)
    (define (divisor-helper current result)
      (cond
        [(> current num) result]
        [else (divisor-helper (add1 current) (+ result (if (zero? (remainder num current)) current 0)))]
        )
      )
    (divisor-helper 2 1)
    )
  (= (divisors-sum a) (divisors-sum b))
  )


(equal? (amicable? 200 300) #f)
(equal? (amicable? 220 284) #t)
(equal? (amicable? 284 220) #t)
(equal? (amicable? 1184 1210) #t)
(equal? (amicable? 2620 2924) #t)
(equal? (amicable? 6232 6368) #t)