#lang racket


(define (my-sorted? x)
  (cond
    [(< x 10) #t]
    [(> (remainder x 10) (remainder (quotient x 10) 10)) #f]
    [else (my-sorted? (quotient x 10))]
    )
  )

(define (sum-numbers a b)
  (if (< b a)
    (sum-numbers b a)
    (apply + (map (λ (x) (if (my-sorted? x) x 0)) (range a (add1 b))))
    )
  )


(sum-numbers 1 9); → 45
(sum-numbers 199 203); → 200
(sum-numbers 219 225); → 663