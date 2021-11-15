#lang racket


(define (num-to-xs x)
  (define (helper result left-over)
    (if (< left-over 10)
        (cons left-over result)
        (helper (cons (remainder left-over 10) result) (quotient left-over 10))
        )
    )
  (helper '() x)
  )

(define (dig-pow n p)
  (let* ([n-xs (num-to-xs n)]
         [res (apply + (map (λ (i) (expt (car (drop n-xs i)) (+ i p))) (range (length n-xs))))])
    (if (zero? (remainder res n)) (quotient res n) -1)
    )
  )


(equal? (dig-pow 89 1) 1)
(equal? (dig-pow 92 1) -1)
(equal? (dig-pow 695 2) 2)
(equal? (dig-pow 46288 3) 51)