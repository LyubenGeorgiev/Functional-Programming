#lang racket

(require math/number-theory)

(define (my-flatten xs)
  (cond
    [(null? xs) '()]
    [(pair? xs) (append (my-flatten (car xs)) (my-flatten (cdr xs)))]
    [else (list xs)]
    )
  )
(equal? (my-flatten '((1 2 3) (4 5 6) ((7 8) (9 10 (11 (12)))))) '(1 2 3 4 5 6 7 8 9 10 11 12))

(define (sum-digit-divisors x)
  (define (helper current)
    (cond
      [(zero? current) 0]
      [(divides? (remainder current 10) x) (+ (remainder current 10) (helper (quotient current 10)))]
      [else (helper (quotient current 10))]
      )
    )
  (helper x)
  )
(= (sum-digit-divisors 1) 1)
(= (sum-digit-divisors 28) 2)
(= (sum-digit-divisors 32) 2)
(= (sum-digit-divisors 29) 0)
(= (sum-digit-divisors 34) 0)
(= (sum-digit-divisors 1048) 13)

(define (same-sum a b)
  (apply + (map (λ (m) (foldl (λ (n res) (if (= (sum-digit-divisors m) (sum-digit-divisors n)) (add1 res) res)) 0 (range (add1 m) (add1 b)))) (range a (add1 b))))
  )
(= (same-sum 28 35) 2)

(define (deep-delete xss)
  (define (helper depth xs)
    (foldr (λ (ys res) (if (list? ys) (cons (helper (add1 depth) ys) res) (if (< ys depth) res (cons ys res)))) '() xs)
    )
  (helper 1 xss)
  )
(equal? (deep-delete '(1 (2 (2 4) 1) 0 (3 (1)))) '(1 (2 (4)) (3 ())))

(define (graph-contains-points xss f)
  (andmap (λ (x) (= (f (car x)) (cdr x))) xss)
  )
(equal? (graph-contains-points '((1 . 2) (2 . 3) (3 . 4)) (λ (x) (+ x 1))) #t)
(equal? (graph-contains-points '((1 . 2) (2 . 4) (3 . 4)) (λ (x) (+ x 1))) #f)

(define (zero-rows xss)
  (map (λ (xs) (if (list? (member 0 xs)) (map (λ (x) 0) xs) xs)) xss)
  )
(equal? (zero-rows '((1 2 0) (3 4 1) (0 5 7) (4 2 4))) '((0 0 0) (3 4 1) (0 0 0) (4 2 4)))