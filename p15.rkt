#lang racket
(require rackunit)
;; p15
(define (repli lst times)
  (cond
    [(null? lst) null]
    [else (append (make-list times (first lst)) (repli (rest lst) times))]))

                
(repli '(a b c) 3)
(check-equal? (repli '(a b c) 3) '(a a a b b b c c c))