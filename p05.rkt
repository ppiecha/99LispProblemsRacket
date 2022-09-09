#lang racket
(require rackunit)

(define (my-reverse lst)
  (define (walk lst buffer)
    (cond
      [(empty? lst) buffer]
      [else (append
             (walk (rest lst) buffer)
             (list (first lst)))]))
  (walk lst '()))

(define (palindrome? lst)
  (equal? lst (my-reverse lst)))

(check-equal? (my-reverse '()) '())
(check-equal? (my-reverse '(a b c)) '(c b a))
  
(check-pred palindrome? '(a b a))
(check-equal? (palindrome? '(a b c)) #f)


