#lang sicp

(define (fringe t)
  (cond ((not (pair? t))
         (if (not (null? t))
             (display t)))
        (else
         (fringe (car t))
         (fringe (cdr t)))))

(define x (list (list 1 2) (list 3 4)))

(fringe (list x x))
