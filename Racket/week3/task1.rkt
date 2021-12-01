#lang racket


(define (rev number)
  (define (rev-helper num res)
    (cond
      [(zero? num) res]
      [else (rev-helper (quotient num 10) (+ (* res 10) (remainder num 10)))]
      )
    )
  (cond
    [(< number 10) number]
    [else (rev-helper (quotient number 10) (remainder number 10))]
    )
  )

(define (num-len number)
  (cond
    [(< number 10) 1]
    [else (add1 (num-len (quotient number 10)))]
    )
  )

(define (remove-first-occurrence number digit)
  (define (remove-first-occurrence-helper prefix sufix)
    (cond
      [(zero? prefix) number]
      [(= (remainder prefix 10) digit) (+ (* (quotient prefix 10) (expt 10 (num-len sufix))) (rev sufix))]
      [else (remove-first-occurrence-helper (quotient prefix 10) (+ (* sufix 10) (remainder prefix 10)))]
      )
    )
  (cond
    [(< number 10) (if (= number digit) 0 number)]
    [(= (remainder number 10) digit) (quotient number 10)]
    [else (remove-first-occurrence-helper (quotient number 10) (remainder number 10))]
    )
  )


(= (remove-first-occurrence 15365 5) 1536)
(= (remove-first-occurrence 15360 0) 1536)
(= (remove-first-occurrence 15300 0) 1530)
(= (remove-first-occurrence 15365 1) 5365)
(= (remove-first-occurrence 35365 3) 3565)
(= (remove-first-occurrence 1212 1) 122)
(= (remove-first-occurrence 1212 2) 121)
(= (remove-first-occurrence (remove-first-occurrence 1212 1) 1) 22)