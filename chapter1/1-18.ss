#lang sicp

(define (double x) (* x 2))

(define (halve x) (/ x 2))

(define (mul a b)
  (define (inner a b sum)
    (cond ((= b 0) sum)
          ((= (remainder b 2) 0) (inner (* a 2) (/ b 2) sum))
          (else (inner a (- b 1) (+ a sum)))))
  (inner a b 0))

(mul 2 8)
(mul 100 99)