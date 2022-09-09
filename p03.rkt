#lang racket
(require rackunit)

(define (element-at lst index)
  (cond
    [(not (positive? index)) "index must be positive"]
    [(< (length lst) index) "out of range"]
    [(<= index 1) (first lst)]
    [else (element-at (rest lst) (sub1 index))]))

(element-at '(a b c d) 0)
(element-at '(a b c d) 1)
(element-at '(a b c d) 2)
(element-at '(a b c d) 5)

(check-equal? (element-at '(a b c d) 2) 'b "second is b")
(check-equal? (element-at '(a b c d) 2) 'c "second is b")

