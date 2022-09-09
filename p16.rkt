#lang racket
(require rackunit)
;; p16
(define (drop lst n-th)
  (define (process lst n-th index result)
    (cond
      [(null? lst) result]
      [(= (remainder index n-th) 0) (append result (process (rest lst) n-th (add1 index) result))]
      [else (append (append result (list (first lst))) (process (rest lst) n-th (add1 index) result))]))
  (process lst n-th 1 null))

         
                
(drop '(a b c d e f g h i k) 3)
(check-equal? (drop '(a b c d e f g h i k) 3) '(a b d e g h k))