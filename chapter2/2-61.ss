#lang sicp

(define (element-of-set? x set)
  (cond ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< x (car set))
         (cons x (adjoin-set x (cdr set))))
        ((> x (car set))
         (cons (car set) (adjoin-set x (cdr set))))
        ((= x (car set)) set)))

(adjoin-set 3 '(1 2 3))
(adjoin-set 1 nil)
