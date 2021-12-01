#lang racket

(define (derive f dx)
  (λ (x) (/ (- (f (+ x dx)) (f x)) dx))
  )

(define (derive-n f n eps)
  (if (zero? n)
      f
      (derive-n (derive f eps) (sub1 n) eps)
      )
  )

(define (derive-n f n dx)
  (λ (x) (if (= n 1)
             ((derive f dx) x)
             ((derive (derive-n f (sub1 n) dx) dx) x)))
  )

(define (repeated f n)
 (lambda (x)
 (if (= n 1)
 (f x)
 (f ((repeated f (- n 1)) x)))))


;((derive-n (λ (x) (* 2 x x x)) 3 1e-3) 2)
(= ((derive-n (λ (x) (* 2 x x x)) 3 1e-3) 2) 12.000015203739167)