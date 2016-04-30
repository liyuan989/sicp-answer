#lang sicp

(define (make-point x y) (cons x y))

(define (x-point point) (car point))

(define (y-point point) (cdr point))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment a b) (cons a b))

(define (mid-segment-point s)
  (cons (/ (+ (x-point (x-point s))
              (x-point (y-point s)))
           2)
        (/ (+ (y-point (x-point s))
              (y-point (y-point s)))
           2)))

(print-point (mid-segment-point (make-segment (make-point 1 2)
                                              (make-point 3 4))))