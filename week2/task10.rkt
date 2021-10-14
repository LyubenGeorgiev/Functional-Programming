#lang racket

(define (automorphic? num)
  (define (helper n nSquared)
    (cond
      [(zero? n) #t]
      [(= (remainder n 10) (remainder nSquared 10)) (helper (quotient n 10) (quotient nSquared 10))]
      [else #f]
      )
    )
  (cond
    [(<= num 1) (error "n was not natural")]
    [else (helper num (* num num))]
    )
  )

(equal? (automorphic? 3)#f)
(equal? (automorphic? 10)#f)
(equal? (automorphic? 5)#t)
(equal? (automorphic? 25)#t)
(equal? (automorphic? 76)#t) 
(equal? (automorphic? 890625)#t) 
(equal? (automorphic? 625)#t) 
(equal? (automorphic? 36) #f)
(equal? (automorphic? 11) #f)
; (automorphic? -1) ; error: n was not natural
; (automorphic? 0) ; error: n was not natural