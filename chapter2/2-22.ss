#lang sicp

(define (reverse table)
  (define (inner t result)
    (if (null? (cdr t))
        (cons (car t) result)
        (inner (cdr t) (cons (car t) result))))
  (inner table nil))

(define (square x) (* x x))

(define (square-list-iter items)
  (define (iter t result)
    (if (null? t)
        result
        (iter (cdr t)
              (cons (square (car t))
                    result))))
  (iter (reverse items) nil))

(square-list-iter (list 1 2 3 4))
