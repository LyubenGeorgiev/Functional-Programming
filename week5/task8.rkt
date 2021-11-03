#lang racket


(define (longest s1 s2)
  (list->string (sort (remove-duplicates (string->list (string-append s1 s2))) (λ (x y) (char<? x y))))
  )

(equal? (longest "xyaabbbccccdefww" "xxxxyyyyabklmopq") "abcdefklmopqwxy")
(equal? (longest "abcdefghijklmnopqrstuvwxyz" "abcdefghijklmnopqrstuvwxyz") "abcdefghijklmnopqrstuvwxyz")