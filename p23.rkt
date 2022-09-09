#lang racket
(require rackunit)
(require racket/random)

#|
P23 (**) Extract a given number of randomly selected elements from a list.
The selected items shall be returned in a list.
Example:
* (rnd-select '(a b c d e f g h) 3)
(E D A)
|#

(define (remove-at lst pos)
  (let rewrite ([lst lst]
                [index 1]
                [result null])
    (cond
      [(null? lst) result] 
      [(= index pos) (rewrite (rest lst) (add1 index) result)]
      [else (rewrite (rest lst) (add1 index) (append result (list (first lst))))])))

(define (rnd-select lst count)
  (let process ([lst lst]
                [del-index (random 1 (add1 (length lst)))]
                [count (- (length lst) count)])
    (cond
      [(= count 0) lst]
      [else (printf "del-index ~a list ~a count ~a\n" del-index lst count)
            (process (remove-at lst del-index) (random 1 (- (length lst) count)) (sub1 count))])))
                
      
(rnd-select '(a b c d e f g h) 3)
;(range2 4 4)
