#lang racket
(require rackunit)

#|
P22 (*) Create a list containing all integers within a given range.
If first argument is smaller than second, produce a list in decreasing order.
Example:
* (range 4 9)
(4 5 6 7 8 9)
|#


(define (range start stop)
  (let rewrite([s-start (if (< start stop) start stop)]
               [s-stop (if (< start stop) stop start)]
               [index (if (< start stop) start stop)]
               [result null])
    (cond
      [(> index s-stop) (if (< start stop) result (reverse result))]
      [(<= index s-stop) (rewrite s-start s-stop (add1 index) (append result (list index)))])))

(define (range2 start stop)
  (let ([start (if (< start stop) start stop)]
        [stop (if (< start stop) stop start)]
        [index (if (< start stop) start stop)]
        [rev (> start stop)])
        (let rewrite([start start]
                     [stop stop]
                     [index index]
                     [result null])
          (cond
            [(> index stop) (if rev (reverse result) result)]
            [(<= index stop) (rewrite start stop (add1 index) (append result (list index)))]))))

      
(range 4 9)
(check-equal? (range 4 9) '(4 5 6 7 8 9))
(range 4 4)
(check-equal? (range 4 4) '(4))
(range 4 2)
(check-equal? (range 4 2) '(4 3 2))
(range2 4 9)
(check-equal? (range2 4 9) '(4 5 6 7 8 9))
(range2 4 4)
(check-equal? (range2 4 4) '(4))
(range2 4 2)
(check-equal? (range2 4 2) '(4 3 2))