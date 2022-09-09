#lang racket
(require rackunit)

#|
P21 (*) Insert an element at a given position into a list.
Example:
* (insert-at 'alfa '(a b c d) 2)
(A ALFA B C D)
|#


(define (remove-at lst pos)
  (let rewrite ([lst lst]
                [index 1]
                [result null])
    (cond
      [(null? lst) result] 
      [(= index pos) (rewrite (rest lst) (add1 index) result)]
      [else (rewrite (rest lst) (add1 index) (append result (list (first lst))))])))

(define (insert-at elem lst pos)
  (let rewrite([lst lst]
               [index 1]
               [result null])
    (cond
      [(null? lst) result]
      [(= index pos) (rewrite (rest lst) (add1 index) (append (append result (list elem (first lst)))))]
      [else (rewrite (rest lst) (add1 index) (append (append result (list (first lst)))))])))

      
(insert-at 'alfa '(a b c d) 2)
(check-equal? (insert-at 'alfa '(a b c d) 2) '(a alfa b c d))