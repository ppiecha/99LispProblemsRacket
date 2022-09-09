#lang racket
(require rackunit)
;; p14
(define (dupli lst)
  (cond
    [(null? lst) null]
    [else (cons (first lst) (cons (first lst) (dupli (rest lst))))]))

(dupli '(a b c c d))
(check-equal? (dupli '(a b c c d)) '(a a b b c c c c d d))
              