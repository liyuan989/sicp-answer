#lang sicp

(define (square x) (* x x))

(define (tree-map factor t)
  (map (lambda (x)
         (if (pair? x)
             (tree-map factor x)
             (factor x)))
       t))

(define (square-tree t)
  (tree-map square t))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(define (tree-map2 factor t)
  (cond ((null? t) nil)
        ((not (pair? t)) (factor t))
        (else
         (cons (tree-map2 factor (car t))
               (tree-map2 factor (cdr t))))))

(define (square-tree2 t)
  (tree-map2 square t))

(square-tree2
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
