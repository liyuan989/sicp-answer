#lang sicp

(define (repeated f n)
  (define (recursive x k)
    (if (= k n)
        (f x)
        (f (recursive x (inc k)))))
  (lambda (x)
    (recursive x 1)))

((repeated (lambda (x) (* x x)) 2) 5)
