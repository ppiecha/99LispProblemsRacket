#lang racket
;;Last in the list
(define (last lst)
  (cond [(empty? lst) "empty list"]
        [(empty? (rest lst)) (first lst)]
        [else (last (rest lst))]))

(last '(1 2 3 4))


        
          

      
  