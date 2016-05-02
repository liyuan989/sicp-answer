#lang sicp

(define (for-each f table)
  (cond ((not (null? table))
         (f (car table))
         (for-each f (cdr table)))))

(for-each (lambda (x)
            (newline)
            (display x))
          (list 27 321 88))
