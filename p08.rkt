#lang racket
(require rackunit)

;;compress
(define (compress lst)
  (define (compressed lst buffer)
    (cond
      [(empty? lst) buffer]
      [(equal? (first lst) (last buffer)) (compressed (rest lst) buffer)]
      [else (compressed (rest lst) (append buffer (list (first lst))))]))
  (cond
    [(empty? lst) empty]
    [else (compressed (rest lst) (list (first lst)))]))
                

(compress '(a a a a b c c a a d e e e e))
(check-equal? (compress '(a a a a b c c a a d e e e e)) '(a b c a d e))
;(check-equal? (my-flatten '(a (b (c d) e))) '(a b c d e))



