#lang racket
(require rackunit)

(define (my-flatten lst)
  (cond
    [(empty? lst) empty]
    [(cons? lst) (append
                  (my-flatten (first lst)) 
                  (my-flatten (rest lst)))]
    [else (list lst)]))

(my-flatten '(a (b (c d) e)))
(check-equal? (my-flatten '()) '())
(check-equal? (my-flatten '(a (b (c d) e))) '(a b c d e))



