#lang sicp

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (cons (+ (xcor-vect v1) (xcor-vect v2))
        (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (cons (- (xcor-vect v1) (xcor-vect v2))
        (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (cons (* s (xcor-vect v))
        (* s (ycor-vect v))))

(define vect1 (make-vect 1 1))
(define vect2 (make-vect 2 2))

(add-vect vect1 vect2)
(sub-vect vect2 vect1)
(scale-vect 3 vect1)
