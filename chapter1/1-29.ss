#lang sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (exact-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (factor k)
    (cond ((or (= k 0) (= k n))
           1)
          ((odd? k)
           4)
          (else
           2)))
  (define (term k)
    (* (factor k)
       (y k)))
  (define (next k) (+ k 1))
  (if (not (even? n))
      (display "n cam't be odd!")
      (* (/ h 3)
         (sum term (exact->inexact 0) next n))))

(define (cube x) (* x x x))

(exact-integral cube 0 1 100)
(exact-integral cube 0 1 1000)
