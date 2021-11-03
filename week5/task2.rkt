#lang racket


(define (get-longest-list xs ys)
  (if (< (length xs) (length ys))
    ys
    xs
    )
  )

(define (longest-ascending-sub xs)
  (define (longest-ascending-subseq-helper head xs)
    (cond
      [(null? xs) '()]
      [(<= head (car xs)) (get-longest-list 
                            (cons (car xs) (longest-ascending-subseq-helper (car xs) (cdr xs)))
                            (longest-ascending-subseq-helper head (cdr xs))
                            )]
      [else (longest-ascending-subseq-helper head (cdr xs))]
      )
    )
  (cond
    [(null? xs) '()]
    [else (get-longest-list
           (cons (car xs) (longest-ascending-subseq-helper (car xs) (cdr xs)))
           (longest-ascending-sub (cdr xs))
           )]
    )
)

(longest-ascending-sub '(1 0 5))
(longest-ascending-sub '(1 5 2 3 1 5 6 7 7 1 5));
(longest-ascending-sub '(1 5 2 3 1 5 2 7 7 15));
(longest-ascending-sub '(1 5 2 3 4 5 6 7 7 1 5));
(longest-ascending-sub '(1 5 2 4 6 8 3 4 1));

(equal? (longest-ascending-sub '(1 0 5)) '(0 5))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 6 7 7 1 5)) '(1 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 2 7 7 15)) '(2 7 7 15))
(equal? (longest-ascending-sub '(1 5 2 3 4 5 6 7 7 1 5)) '(2 3 4 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 4 6 8 3 4 1)) '(2 4 6 8))