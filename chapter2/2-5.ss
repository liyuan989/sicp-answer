#lang sicp

(define (expt base n)
  (define (iter base k val)
    (if (= k n)
        val
        (iter base (inc k) (* base val))))
  (iter base 0 1))

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (if (= (remainder z 2) 0)
      0
      (+ 1 (car (/ z 2)))))

(define (cdr z)
  (if (= (remainder z 3) 0)
      0
      (+ 1 (cdr (/ z 3)))))

(car (cons 1 2))
(cdr (cons 1 2))