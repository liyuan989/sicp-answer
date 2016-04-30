#lang sicp

(define (sign a b)
  (if (< (* a b) 0)
      -1
      1))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((factor (gcd n d))
        (s (sign n d)))
    (cons (* s (abs (/ n factor)))
          (abs (/ d factor)))))

(define (number x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (number x))
  (display "/")
  (display (denom x)))

(print-rat (make-rat 6 9))
(print-rat (make-rat -6 9))
(print-rat (make-rat 6 -9))
(print-rat (make-rat -6 -9))