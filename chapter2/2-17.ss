#lang sicp

(define (last-pair table)
  (if (null? (cdr table))
      (car table)
      (last-pair (cdr table))))

(last-pair (list 23 72 148 34))
