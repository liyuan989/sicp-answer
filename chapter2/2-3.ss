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

(define (rectangle a b) (cons a b))

(define (long rect)
  (- (x-point (y-point (x-point rect)))
     (x-point (x-point (x-point rect)))))

(define (wide rect)
  (- (y-point (x-point (x-point rect)))
     (y-point (x-point (y-point rect)))))

(define (area rect)
  (* (long rect) (wide rect)))

(define (perimeter rect)
  (* 2 (+ (long rect) (wide rect))))

(define r (rectangle (make-segment (make-point 1 4) (make-point 4 4))
                     (make-segment (make-point 1 -1) (make-point 4 -1))))

(area r)
(perimeter r)