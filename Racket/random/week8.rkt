#lang racket


(define (zero-cols xss)
  (apply map list (apply map (λ args (if (ormap zero? args) (map (λ (x) 0) args) args)) xss))
  )

(equal? (zero-cols '((1 2 0) (3 4 1) (0 5 7) (4 2 4))) '((0 2 0) (0 4 0) (0 5 0) (0 2 0)))


(define (prod xs) (apply * xs))
(define (sum xs) (apply + xs))

(define (best-metric? ms xss)
  (null? (cdr (remove-duplicates (apply map (λ args (cdr (argmax car args))) (map (λ (metric) (map (λ (xs) (cons (metric xs) metric)) xss)) ms)))))
  )

(equal? (best-metric? (list sum prod) `((1337 0) (3 -4 5) (0 1 2))) #t)
(equal? (best-metric? (list car sum) `((100 -100) (29 1) (42))) #f)

(define (tabulate f)
  (λ (a b) (map (λ (x) (cons x (f x))) (range a (add1 b))))
  )

(equal? ((tabulate (λ (x) (* x x))) 1 5) '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25)))

(define (naive-levenshtein a b)
  (let ([i (length a)] [j (length b)])
    (if (zero? (min i j))
      (max i j)
      (min (add1 (naive-levenshtein (cdr a) b)) (add1 (naive-levenshtein a (cdr b))) (+ (if (equal? (car a) (car b)) 0 1) (naive-levenshtein (cdr a) (cdr b))))
      )
    )
  )

(= (naive-levenshtein '(c a t) '(d o g)) 3)
(= (naive-levenshtein '(c a t) '(h a t)) 1)
(= (naive-levenshtein '(k i t t e n) '(w r i t t e n)) 2)


(define (find-closest-words words)
  (let* ([all-pairs (foldl (λ (word res) (append res (cartesian-product (list word) (remove word words)))) '() words)]
         [min-el (argmin (λ (x) (naive-levenshtein (car x) (cadr x))) all-pairs)]
         [min-len (naive-levenshtein (car min-el) (cadr min-el))])
    (remove-duplicates (filter (λ (x) (equal? min-len (naive-levenshtein (car x) (cadr x)))) all-pairs) (λ (a b) (equal? a (append (cdr b) (list (car b)))))))
  )

(equal? (find-closest-words '((c a t) (k i t t e n) (w r i t t e n) (h a t) (b a t))) '(((c a t) (h a t)) ((c a t) (b a t)) ((h a t) (b a t))))


(define (diagonal xss)
  (map (λ (i) (car (drop (car (drop xss i)) i))) (range (min (length xss) (length (car xss)))))
  )

(equal? (diagonal '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16))) '(1 6 11 16))