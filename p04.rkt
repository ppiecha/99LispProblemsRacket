#lang racket
(require rackunit)

(define (my-length lst)
  (define (iter lst len)
    (cond[(empty? lst) len]
         [else (printf "lst ~a len ~a\n" lst len)
               (iter (rest lst) (add1 len))]))
  (iter lst 0))

(check-equal? (my-length '(a aa b bb ccc)) 5)
(check-equal? (my-length '()) 0)

