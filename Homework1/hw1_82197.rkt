#lang racket


(define (sum-of-digits x)
  (cond
    [(< x 10) x]
    [else (+ (remainder x 10) (sum-of-digits (quotient x 10)))]
    )
  )

(define (count-digit number digit)
  (cond
    [(< number 10) (if (= number digit) 1 0)]
    [(= digit (remainder number 10)) (add1 (count-digit (quotient number 10) digit))]
    [else (count-digit (quotient number 10) digit)]
    )
  )

(define (sum-counts-iter to digit)
  (define (counts-iter current result)
    (cond
      [(> current to) result]
      [else (counts-iter (add1 current) (+ result (count-digit current digit)))]
      )
    )
  (cond
    [(< to 1) (error "Expected a natural number >= 1 in sum-counts-iter")]
    [else (sum-of-digits (counts-iter 1 0))]
    )
  )

(define (add-ones number)
  (define (add-ones-helper num result counter)
    (cond
      [(= num 0) result]
      [(= (remainder num 10) 9) (add-ones-helper (quotient num 10) (+ result (expt 10 (add1 counter))) (+ 2 counter))]
      [else (add-ones-helper (quotient num 10) (+ result(* (add1 (remainder num 10)) (expt 10 counter))) (+ 1 counter))]
      )
    )
  (cond
    [(< number 10) (add1 number)]
    [else (add-ones-helper number 0 0)]
    )
  )

(= (sum-counts-iter 1 1) 1)
(= (sum-counts-iter 5123 1) 19)
(= (sum-counts-iter 1234 8) 10)
(= (sum-counts-iter 5555 5) 10)
(= (sum-counts-iter 65432 6) 11)
(= (sum-counts-iter 70000 1) 11)
(= (sum-counts-iter 123321 1) 29)

(= (add-ones 123) 234)
(= (add-ones 193) 2104)
(= (add-ones 998) 10109)
(= (add-ones 9999) 10101010)