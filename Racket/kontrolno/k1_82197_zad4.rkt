#lang racket

(define (prod-digits x)
  (cond
    [(< x 10) x]
    [else (* (remainder x 10) (prod-digits (quotient x 10)))]
    )
  )

(define (persistence x)
  (define (get-list n)
    (cond
      [(< n 10) (list n)]
      [(< (prod-digits n) 10) (list (prod-digits n))]
      [else (cons (prod-digits n) (get-list (prod-digits n)))]
      )
    )
  (cons (get-list x) (length (get-list x)))
  )


(persistence 39); → '((27 14 4) . 3) ; 3*9=27, 2*7=14, 1*4=4
(persistence 126); → '((12 2) . 2) ; 1*2*6=12, 1*2=2
(persistence 4); → '((4) . 1)
(persistence 999); → '((729 126 12 2) . 4)