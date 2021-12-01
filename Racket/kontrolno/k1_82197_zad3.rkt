#lang racket

(define (get-zeroes n)
  (define (helper current-pow)
    (cond
      [(> current-pow n) 0]
      [else (+ (quotient n current-pow) (helper (* current-pow 5)))]
      )
    )
  (helper 5)
  )

(define (trailing-zeros n)
  (λ (pred) (pred (get-zeroes n)))
  )



((trailing-zeros 6) even?); → #f (броят на влачещите нули
;е 1. 1 не е четно)
((trailing-zeros 1000) even?); → #f (броят на влачещите нули
;е 249. 249 не е четно)
((trailing-zeros 100000) even?); → #f (броят на влачещите нули
;е 24999. 24999 не е четно)
((trailing-zeros 1000000000) even?); → #t (броят на влачещите нули
;е 249999998. 249999998 е четно)