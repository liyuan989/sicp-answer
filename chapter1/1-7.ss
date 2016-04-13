#lang sicp

;original sqrt
(define (improve guess x)
  (average guess (/ x guess)))

(define (square x) (* x x))

(define (average x y) (/ (+ x y) 2))

(define (sqrt-iter guess x)
  (define (good-enough? quess x)
  (< (abs(- (square quess) x))
     0.001))
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(sqrt-iter 1.0 4)
(sqrt-iter 1.0 0.00001)
;(sqrt-iter 1.0 99999999999999999999999999999999999999999999999999999999999999)

;improved sqrt
(define (sqrt-improved guess x)
  (define (good-enough? new-guess old-guess)
    (< (/ (abs (- new-guess old-guess))
          old-guess)
       0.01))
  (if (good-enough? guess (improve guess x))
      (improve guess x)
      (sqrt-improved (improve guess x)
                     x)))

(sqrt-improved 1.0 9)
(sqrt-improved 1.0 0.00001)
(sqrt-improved 1.0 99999999999999999999999999999999999999999999999999999999999999)