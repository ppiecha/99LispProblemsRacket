#lang racket
(require rackunit)
;; p17
#|
P17 (*) Split a list into two parts; the length of the first part is given.
Do not use any predefined functions.

Example:
* (split '(a b c d e f g h i k) 3)
( (A B C) (D E F G H I K))
|#

(define (split lst len)
  (define (process lst len index f-lst s-lst)
    (cond
      [(null? lst) (list f-lst s-lst)]
      [(<= index len) (process (rest lst) len (add1 index) (append f-lst (list (first lst))) s-lst)]
      [else (process (rest lst) len (add1 index) f-lst (append s-lst (list (first lst))))]))
  (process lst len 1 '() '()))
         
                
(split '(a b c d e f g h i k) 3)
(check-equal? (split '(a b c d e f g h i k) 3) '((a b c) (d e f g h i k)))