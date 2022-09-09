#lang racket
(define (last-by-one lst)
  (cond
    [(< (length lst) 2) "too short"]
    [(empty? (rest (rest lst))) (list (first lst) (second lst))]
    [else (last-by-one (rest lst))]))

(last-by-one '(1))
(last-by-one '(1 2))
(last-by-one '(1 2 3))
(last-by-one '(1 2 3 4))
     