#lang racket
(require rackunit)
;; p09
(define (pack lst)
  (define (eat lst result sub-list)
    (cond
      [(empty? lst) (append result (list sub-list))]
      [(equal? (first lst) (first sub-list)) (eat (rest lst) result (append sub-list (list (first lst))))]
      [else (eat (rest lst) (append result (list sub-list)) (list (first lst)))]))
  (cond
    [(empty? lst) empty]
    [else (eat (rest lst) empty (list (first lst)))]))

(pack '(a a a a b c c a a d e e e e))
;; p10
(define (encode lst)
  (define (apply func lst)
    (cond
      [(empty? lst) empty]
      [else (cons (func (first lst)) (apply func (rest lst)))]))
  (apply (λ (x) (list (length x) (first x))) (pack lst)))

(encode '(a a a a b c c a a d e e e e))

(define (encoding item)
  (cond
    [(= (length item) 1) (first item)]
    [else (list (length item) (first item))]))

;; p11
(define (encode-modified lst)
  (define (apply func lst)
    (cond
      [(empty? lst) empty]
      [else (cons (func (first lst)) (apply func (rest lst)))]))
  (apply encoding (pack lst)))

(encode-modified '(a a a a b c c a a d e e e e))

;(define (test-let1 lst)
  

;; p12
;#|
(define (decode lst)
  (let ([process-item (λ (item)
                        (if (cons? item)
                            (make-list (first item) (second item))
                            (list item)))])
    (cond
      [(empty? lst) empty]
      [else (append (process-item (first lst)) (decode (rest lst)))])))

(decode(encode-modified '(a a a a b c c a a d e e e e)))