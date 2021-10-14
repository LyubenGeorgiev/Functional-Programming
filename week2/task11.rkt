#lang racket

(define (prime? n)
  (define (f curDiv)
    (cond
      [(< n (* curDiv curDiv)) #t]
      [(zero? (remainder n curDiv)) #f]
      [else (f (add1 curDiv))]
      )
    )
  (f 2)
  )

(define (nth-cubic n)
  ; checks if a number X can be represented as Y * (Y + 1)
  (define (special-composed number)
    (= number (* (floor (sqrt number)) (add1 (floor (sqrt number)))))
    )
  (define (helper index current)
    (if (and (prime? (add1 (* index 3))) (special-composed index))
      (if (= current n)
        (add1 (* index 3))
        (helper (add1 index) (add1 current))
        )
      (helper (add1 index) current)
      )
    )
  (cond
    [(<= n 0) (error "n was not natural")]
    [else (helper 2 1)]
    )
  )


(= (nth-cubic 1) 7)
(= (nth-cubic 4) 61)
(= (nth-cubic 50) 55897)
(= (nth-cubic 100) 283669)
(= (nth-cubic 200) 1570357)
; (nth-cubic 0) ; should return an error