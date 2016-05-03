#lang sicp

(define (square x) (* x x))

(define (square-tree t)
  (map (lambda (x)
         (if (pair? x)
             (square-tree x)
             (square x)))
       t))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(define (square-tree2 t)
  (cond ((null? t) nil)
        ((not (pair? t)) (square t))
        (else
         (cons (square-tree2 (car t))
               (square-tree2 (cdr t))))))

(square-tree2
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
