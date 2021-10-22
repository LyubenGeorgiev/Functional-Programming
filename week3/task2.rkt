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

(define (find-max n)
  (define (helper current-n max)
    (cond
      [(zero? current-n) max]
      [(> (remainder current-n 10) max) (helper (quotient current-n 10) (remainder current-n 10))]
      [else (helper (quotient current-n 10) max)]
      )
    )
  (helper (quotient n 10) (remainder n 10))
  )

(define (count-zeros num)
  (define (count-zeros-helper n)
    (cond
      [(zero? n) 0]
      [(zero? (remainder n 10)) (add1 (count-zeros-helper (quotient n 10)))]
      [else (count-zeros-helper (quotient n 10))]
      )
    )
  (cond
    [(zero? num) 1]
    [else (count-zeros-helper num)]
    )
  )

(define (sort-n n)
  (define (sort-n-helper current result)
    (cond
      [(zero? current) result]
      [else (sort-n-helper (remove-first-occurrence current (find-max current)) (+ (* result 10) (find-max current)))]
      )
    )
  (cond
    [(< n 10) n]
    [else (* (expt 10 (count-zeros n)) (sort-n-helper (remove-first-occurrence n (find-max n)) (find-max n)))]
    )
  )

(= (sort-n 1714) 7411)
(= (sort-n 123450) 543210)
(= (sort-n 123405) 543210)
(= (sort-n 123045) 543210)
(= (sort-n 120345) 543210)
(= (sort-n 102345) 543210)
(= (sort-n 8910) 9810)
(= (sort-n 321) 321)
(= (sort-n 29210) 92210)
(= (sort-n 1230) 3210)
(= (sort-n 55345) 55543)
(= (sort-n 14752) 75421)
(= (sort-n 329450) 954320)
(= (sort-n 9125) 9521)