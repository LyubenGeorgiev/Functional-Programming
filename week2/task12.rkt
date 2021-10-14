#lang racket

(define (two-to-the-power-of x)
  (define (helper cur)
    (cond
      [(= cur x) 1]
      [else (* 2 (helper (add1 cur)))]
      )
    )
  (helper 0)
  )

(define (find-sum a b n)
  (+ (* 3 a) (* b (- (two-to-the-power-of (add1 n)) (+ 3 (two-to-the-power-of (- n 2))))))
  )


(= (find-sum 0 2 10) 3578) ; 510 + 1022 + 2046
(= (find-sum 5 3 5) 174) ; 26 + 50 + 98