#lang racket
(require rackunit)

#|
P19 (**) Rotate a list N places to the left.
Examples:
* (rotate '(a b c d e f g h) 3)
(D E F G H A B C)

* (rotate '(a b c d e f g h) -2)
(G H A B C D E F)

Hint: Use the predefined functions length and append, as well as the result of problem P17

|#

(define (split lst len)
  (define (process lst len index f-lst s-lst)
    (cond
      [(null? lst) (list f-lst s-lst)]
      [(<= index len) (process (rest lst) len (add1 index) (append f-lst (list (first lst))) s-lst)]
      [else (process (rest lst) len (add1 index) f-lst (append s-lst (list (first lst))))]))
  (process lst len 1 '() '()))


(define (rotate lst pos)
  (let* ([pos (if (> pos 0) pos (+ (length lst) pos))]
         [parts (split lst pos)]
         [move (λ (parts) (append (second parts) (first parts)))])
    (printf "pos ~a\n" pos)
    (move parts)))
         

(rotate '(a b c d e f g h) 3)
(check-equal? (rotate '(a b c d e f g h) 3) '(d e f g h a b c))
(rotate '(a b c d e f g h) -2)
(check-equal? (rotate '(a b c d e f g h) -2) '(g h a b c d e f))