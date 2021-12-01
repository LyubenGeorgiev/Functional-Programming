#lang racket


(define (calc-series-sum x to)
  (define (f n divider)
    (/ (* (expt 2 (add1 n)) (expt x n)) divider)
    )
  (define (calc-series-sum-helper current special-divider)
    (cond
      [(> current to) 0]
      [(zero? (remainder current 2)) (-
                                      (calc-series-sum-helper (add1 current) (* special-divider (add1 (* (add1 current) 2))))
                                      (f current special-divider))]
      [else (+ (calc-series-sum-helper (add1 current) (* special-divider (add1 (* (add1 current) 2)))) (f current special-divider))]
      )
    )
  (- (calc-series-sum-helper 1 3) 2)
  )



(calc-series-sum 1 0) ; -2
(calc-series-sum 1 1) ; -2/3
(calc-series-sum 1 2) ; -1 1/5
(calc-series-sum 1 3) ; -1 1/21
(calc-series-sum 1 4) ; -1 11/135
(calc-series-sum 1 5) ; -1 29/385
(calc-series-sum 1 6) ; -1 937/12285