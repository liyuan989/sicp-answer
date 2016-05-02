#lang sicp

(define (reverse table)
  (define (inner t result)
    (if (null? (cdr t))
        (cons (car t) result)
        (inner (cdr t) (cons (car t) result))))
  (inner table '()))

(reverse (list 1 4 9 16 25))
