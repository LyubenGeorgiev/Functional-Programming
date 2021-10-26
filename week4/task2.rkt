#lang racket

(define (repeater str)
  (λ (count glue) (cond
                    [(= count 1) str]
                    [else (string-append str glue ((repeater str) (sub1 count) glue))]
                    ))
  )


(equal? ((repeater "I love Racket") 3 " ") "I love Racket I love Racket I love Racket")
(equal? ((repeater "Quack") 5 "!") "Quack!Quack!Quack!Quack!Quack")