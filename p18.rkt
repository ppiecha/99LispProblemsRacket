#lang racket
(require rackunit)

#|
P18 (**) Extract a slice from a list.
Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.

Example:
* (slice '(a b c d e f g h i k) 3 7)
(C D E F G)
|#

(define (slice lst start stop)
  (define (process lst start stop index result)
    (cond
      [(null? lst) result]
      [(and (>= index start) (<= index stop))
           (process (rest lst) start stop (add1 index) (append result (list (first lst))))]
      [else (process (rest lst) start stop (add1 index) result)]))
  (process lst start stop 1 '()))

(slice '(a b c d e f g h i k) 3 7)
(check-equal? (slice '(a b c d e f g h i k) 3 7) '(c d e f g))