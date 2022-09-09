#lang racket
#|
P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list
In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.

Example:
* (combination 3 '(a b c d e f))
((A B C) (A B D) (A B E) ... )
|#

;; (a)
;; (a b) 
;; (a b c)
#|
(define (combination cnt lst)
  let iter ([cnt cnt]
            [lst lst]
            [result '()]
  (cond
    [(null? lst) result]
    [(> cnt (length lst)) (raise-arguments-error ('combination "wrong args" "list" lst "count" cnt))]
    [(= cnt (length lst)) (list lst)]
    [else (printf "count ~a list ~a result ~a\n" cnt lst result)
          (combination (
          ((append (list (first lst)) (combination (sub1 cnt) (rest lst)))
           (combination cnt (rest lst)))]))

(combination 2 '(a b c))
|#

(define (test-match lst)
  (match lst
    ['() 'empty]
    [(cons h t) h]))

(define (test-match2 lst n)
  (match (length lst)
    [(== n) 'equal]
    [_ 'not-equal]))

;;(test-match '())
;;(test-match '(a b))

;;(test-match2 '() 2)
;;(test-match2 '(a b) 2)
;;(length '())

(define (n-first lst n)
  (let loop ([lst lst]
             [n n]
             [index 0]
             [result '()])
    (match (length result)
      [(== n) result]
      [_ #(printf "lst ~a n ~a result ~a\n" lst n result)
         (loop (rest lst) n (add1 index) (append result (list (first lst))))])))

;;(n-first '(a b c d) 2)
;;(n-first '(a b c d) 4)

(define (fill-rest head tail result)
  (match tail
    ['() result]
    [(cons h t)
     #(printf "head ~a tail ~a result ~a\n" h t result)
     (fill-rest head
                t
                (append result (list (append head (list h)))))]))

;(fill-rest '(a b) '(c d) '())
;(fill-rest '(a) '(b c d) '())

;#|
(define (combination cnt lst)
  (define (fill-rest head tail result)
    (match tail
      ['() result]
      [(cons h t)
       #(printf "head ~a tail ~a result ~a\n" h t result)
       (fill-rest head
                  t
                  (append result (list (append head (list h)))))]))
  (define lst-length (length lst))
  (let loop ([lst lst]
             [cnt cnt]
             [head (list (first lst))]
             [result '()])
    (match lst
      [(cons h t)
       #:when (< (- lst-length (length head)) cnt)
       result]
      [(cons h t)
       (printf "lst ~a cnt ~a head ~a result ~a\n" lst cnt head result)
       (loop t
             (sub1 cnt)
             (append head (list (first t)))
             (append result (fill-rest head t result)))])))

(combination 2 '(a b c))
;|#

                     
  
