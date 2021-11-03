#lang racket


(define (get-longest-list xs ys)
  (if (< (length xs) (length ys))
    ys
    xs
    )
  )

(define (longest-ascending-not-consecutive-sub xs)
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
           (longest-ascending-not-consecutive-sub (cdr xs))
           )]
    )
)


;(equal? (longest-ascending-not-consecutive-sub '(1 0 5)) '(1 5))
;(equal? (longest-ascending-not-consecutive-sub '(1 5 2 3 1 5 6 7 7 1 5)) '(1 2 3 5 6 7 7))
;(equal? (longest-ascending-not-consecutive-sub '(1 5 2 3 1 5 2 7 7 15)) '(1 2 3 5 7 7 15))
;(equal? (longest-ascending-not-consecutive-sub '(1 5 2 3 4 5 6 7 7 1 5)) '(1 2 3 4 5 6 7 7))
;(equal? (longest-ascending-not-consecutive-sub '(1 5 2 4 6 8 3 4 1)) '(1 2 4 6 8))

(define (longest-ascending-sub xs)
  (define (helper body max-len cur-len best-list cur-list)
    (cond
      [(null? body) (cond
              [(< max-len cur-len) cur-list]
              [else best-list]
              )]
      [(>= (car body) (car cur-list)) (helper (cdr body) max-len (add1 cur-len) best-list (cons (car body) cur-list))]
      [else (cond
              [(< max-len cur-len) (helper (cdr body) cur-len 1 cur-list (list (car body)))]
              [else (helper (cdr body) max-len 1 best-list (list (car body)))]
              )]
      )
    )
  (cond
    [(null? xs) '()]
    [else (reverse (helper (cdr xs) 1 1 (list (car xs)) (list (car xs))))]
    )
  )


(equal? (longest-ascending-sub '(1 0 5)) '(0 5))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 6 7 7 1 5)) '(1 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 2 7 7 15)) '(2 7 7 15))
(equal? (longest-ascending-sub '(1 5 2 3 4 5 6 7 7 1 5)) '(2 3 4 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 4 6 8 3 4 1)) '(2 4 6 8))