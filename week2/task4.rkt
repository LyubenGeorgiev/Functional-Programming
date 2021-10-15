#lang racket

(define (rev n)
  (define (rev-helper curNum result)
    (cond
      [(zero? curNum) result]
      [else (rev-helper (quotient curNum 10) (+ (* result 10) (remainder curNum 10)))]
      )
    )
  (rev-helper (quotient n 10) (remainder n 10))
  )

(define (palindrome? num)
    (= num (rev num))
    )

(define (num-palindromes a b)
  (define (palindrome-counter-helper current res)
    (cond
      [(= current b) (+ res (if (palindrome? current) 1 0))]
      [else (palindrome-counter-helper (add1 current) (+ res (if (palindrome? current) 1 0)))]
      )
    )
  (if (< b a)
      (num-palindromes b a)
      (palindrome-counter-helper a 0)
    )
  )

(= (num-palindromes 1 101) 19)
(= (num-palindromes 1 100) 18)
(= (num-palindromes 100 1) 18)