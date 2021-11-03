#lang racket


(define (remove-first-no-proc x xs)
  (cond
    [(null? xs) xs]
    [(equal? (car xs) x) (cdr xs)]
    [else (cons (car xs) (remove-first-no-proc x (cdr xs)))]
    )
  )

(define (remove-all-no-proc x xs)
  (cond
    [(not (list? x)) (remove-all-no-proc (list x) xs)]
    [(null? x) xs]
    [(equal? (remove-first-no-proc (car x) xs) xs) (remove-all-no-proc (cdr x) xs)]
    [else (remove-all-no-proc x (remove-first-no-proc (car x) xs))]
    )
  )

(define (remove-all-proc x xs)
  (remq* (list x) xs)
  )


; without using a predefined procedure
(equal? (remove-all-no-proc 1 '(1 1 1 2)) '(2))
(equal? (remove-all-no-proc 1 '(2 5 6)) '(2 5 6))
(equal? (remove-all-no-proc 1 '(1)) '())
(equal? (remove-all-no-proc 1 '(1 2 1 1)) '(2))
(equal? (remove-all-no-proc "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN"))

; using a predefined procedure
(equal? (remove-all-proc 1 '(1 1 1 2)) '(2))
(equal? (remove-all-proc 1 '(2 5 6)) '(2 5 6))
(equal? (remove-all-proc 1 '(1)) '())
(equal? (remove-all-proc 1 '(1 2 1 1)) '(2))
(equal? (remove-all-proc "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN"))