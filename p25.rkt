#lang racket
#|
P25 (*) Generate a random permutation of the elements of a list.
Example:
* (rnd-permu '(a b c d e f))
(B A D C E F)
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


(define (rnd-select2 lst cnt)
  (let iter([lst lst]
            [cnt cnt]
            [result null])
    (let* ([rnd (random 0 (length lst))]
           [elem (list (list-ref lst rnd))]
           [trimmed (remove-at lst (add1 rnd))])
      (cond
        [(= cnt 0) result]
        [(and (= cnt 1) (= (length lst) 1)) (append result lst)] 
        [else
         #(printf "lst ~a cnt ~a result ~a\n" lst cnt result)
         (iter trimmed (sub1 cnt) (append result elem))]))))
    


(define (rnd-permu lst)
  (rnd-select2 lst (length lst)))

(rnd-select2 '(a b c d e f) 3)
(rnd-select2 '(a b c d e f) 4)
(rnd-select2 '(a b c d e f) 5)
(rnd-select2 '(a b c d e f) 6)
(rnd-permu '(a b c d e f))



