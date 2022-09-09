#lang racket
(require rackunit)

#|
P20 (*) Remove the K'th element from a list.
Example:
* (remove-at '(a b c d) 2)
(A C D)
|#


(define (remove-at lst pos)
  (let rewrite ([lst lst]
                [index 1]
                [result null])
    (cond
      [(null? lst) result] 
      [(= index pos) (rewrite (rest lst) (add1 index) result)]
      [else (rewrite (rest lst) (add1 index) (append result (list (first lst))))])))
        
    
      
(remove-at '(a b c d) 2)
(check-equal? (remove-at '(a b c d) 2) '(a c d))