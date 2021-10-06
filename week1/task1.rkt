#lang racket

(define (my-gcd x y)
    (cond
        [(= x 0) y]
        [(= y 0) x]
        [else
            (if (< x y)
                (my-gcd y x)
                (my-gcd y (- x (* (quotient x y) y)))        
              )]
      )
    
  )

(= (my-gcd 5 13) 1)
(= (my-gcd 13 1235) 13)