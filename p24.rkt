#lang racket

#|
P24 (*) Lotto: Draw N different random numbers from the set 1..M.
The selected numbers shall be returned in a list.
Example:
* (lotto-select 6 49)
(23 1 17 33 21 37)
|#

(define (lotto-select count max)
  (let iter ([result null]
             [missing count])
    (cond
      [(= missing 0) result]
      [else (let ([rand (add1 (random max))])
              (if (member rand result)
                  (iter result missing)
                  (iter (append result (list rand)) (sub1 missing))))])))

(lotto-select 6 49)
