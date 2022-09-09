#lang racket
(require rackunit)
;; p13
(define (decode2 lst)
  (define (process lst count elem)
    (let ([item (if (= count 1) elem (list count elem))])
      (cond
        [(empty? lst) (cons item empty)]
        [(equal? (first lst) elem) (process (rest lst) (add1 count) elem)]
        [else (cons item (process (rest lst) 1 (first lst)))])))
  (cond
    [(empty? lst) empty]
    [else (process (rest lst) 1 (first lst))]))
                
(decode2 '(a a a a b c c a a d e e e e))
(check-equal? (decode2 '(a a a a b c c a a d e e e e)) '((4 a) b (2 c) (2 a) d (4 e)))

;;(printf "lst ~a len ~a\n" lst len)