#lang racket

(require math/number-theory)

(define (sum-prime-divs number)
  (define (helper curNum)
    (cond
      [(> curNum number) 0]
      [(and (zero? (remainder number curNum)) (prime? curNum)) (+ curNum (helper (add1 curNum)))]
      [else (helper (add1 curNum))]
      )
    )
  (if (< number 0)
    (error "n was negative")
    (helper 2)
    )
  )

(= (sum-prime-divs 0) 0)
(= (sum-prime-divs 6) 5) ; 2 + 3
(= (sum-prime-divs 18) 5) ; 2 + 3
(= (sum-prime-divs 19) 19)
(= (sum-prime-divs 45136) 53)