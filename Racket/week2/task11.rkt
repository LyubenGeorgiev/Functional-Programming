#lang racket

(require math/number-theory)

(define (nth-cubic n)
  (define (helper index current)
    (if (prime? (add1 (- (* 3 index index) (* 3 index))))
        (if (= current n)
          (add1 (- (* 3 index index) (* 3 index)))
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
(= (nth-cubic 1000) 65524807)
(= (nth-cubic 10000) 11712188419)
; (nth-cubic 0) ; should return an error