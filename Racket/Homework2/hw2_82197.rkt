#lang racket


(define (itinerary flights)
  (define (my-min a b)
    (let ([stringify (λ (x) (foldl (λ (item res) (string-append res (cdr item))) (caar x) x))])
      (if (string<? (stringify a) (stringify b)) a b)))
  (λ (start) (let* ([options (filter (λ (nf) (and (equal? (caar nf) start)
                                                  (andmap (λ (a b) (equal? (cdr a) (car b))) (take nf (sub1 (length flights))) (cdr nf)))) (permutations flights))]
                    [result (if (null? options) "No such itinerary!" (foldl (λ (cur best) (my-min cur best)) (car options) (cdr options)))])
               (if (list? result) (reverse (foldl (λ (current res) (cons (cdr current) res)) (list (caar result)) result)) result))))


(define (pad xss)
  (λ (x) (let* ([x (list x)] [wrap (λ (yss) (map (λ (ys) (append x ys x)) yss))]) (apply map list (wrap (apply map list (wrap xss)))))))

(equal? ((itinerary '(("SFO" . "HKO") ("YYZ" . "SFO") ("YUL" . "YYZ") ("HKO" . "ORD"))) "YUL") '("YUL" "YYZ" "SFO" "HKO" "ORD"))

(equal? ((itinerary '(("A" . "B") ("A" . "C") ("B" . "C") ("C" . "A"))) "A") '("A" "B" "C" "A" "C"))

(equal? ((itinerary '(("SFO" . "COM") ("COM" . "YYZ"))) "COM") "No such itinerary!")

(equal? ((itinerary '(("Pleven" . "Vidin") ("Kurdjali" . "Pleven") ("Tsarevets" . "Kurdjali") ("Svoge" . "Tsarevets") ("Pleven" . "Tsarevets") ("Vidin" . "Svoge"))) "Pleven")
        '("Pleven" "Tsarevets" "Kurdjali" "Pleven" "Vidin" "Svoge" "Tsarevets"))

(equal? ((itinerary '(("G" . "E") ("E" . "F") ("A" . "G") ("D" . "E") ("B" . "C") ("E" . "C") ("A" . "B") ("C" . "D") ("F" . "A") ("C" . "A"))) "A")
        '("A" "B" "C" "A" "G" "E" "C" "D" "E" "F" "A"))


(equal? ((pad
  '( (1 2 3)
     (4 5 6)
     (7 8 9) )) 0)
  '( (0 0 0 0 0)
     (0 1 2 3 0)
     (0 4 5 6 0)
     (0 7 8 9 0)
     (0 0 0 0 0) ) )

(equal? ((pad
  '( (1 2 3)
   (4 5 6)
   (7 8 9) )) 9)
  '( (9 9 9 9 9)
   (9 1 2 3 9)
   (9 4 5 6 9)
   (9 7 8 9 9)
   (9 9 9 9 9) ))