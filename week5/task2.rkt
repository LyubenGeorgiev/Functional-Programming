#lang racket


(define (longest-ascending-not-consecutive-sub xs)
  (argmax length (filter (λ (ys) (equal? ys (sort ys <))) (combinations xs))))

(equal? (longest-ascending-not-consecutive-sub '(1 0 5)) '(1 5))
(equal? (longest-ascending-not-consecutive-sub '(1 5 2 3 1 5 6 7 7 1 5)) '(1 2 3 5 6 7 7))
(equal? (longest-ascending-not-consecutive-sub '(1 5 2 3 1 5 2 7 7 15)) '(1 2 3 5 7 7 15))
(equal? (longest-ascending-not-consecutive-sub '(1 5 2 3 4 5 6 7 7 1 5)) '(1 2 3 4 5 6 7 7))
(equal? (longest-ascending-not-consecutive-sub '(1 5 2 4 6 8 3 4 1)) '(1 2 4 6 8))

(define (longest-ascending-sub xs)
  (if (null? xs)
    xs
    (reverse (argmax length (foldl (λ (cur res) (if (< cur (caar res)) (cons (list cur) res) (cons (cons cur (car res)) (cdr res)))) (list (list (car xs))) (cdr xs))))))


(equal? (longest-ascending-sub '(1 0 5)) '(0 5))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 6 7 7 1 5)) '(1 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 2 7 7 15)) '(2 7 7 15))
(equal? (longest-ascending-sub '(1 5 2 3 4 5 6 7 7 1 5)) '(2 3 4 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 4 6 8 3 4 1)) '(2 4 6 8))