#lang racket


(define (repeater str)
  (λ (count glue) (if (zero? count) "" (string-append str (apply string-append (map (λ (x) (string-append glue str)) (range (sub1 count)))))))
  )


((repeater "I love Racket") 3 " ")
((repeater "Quack") 5 "!")