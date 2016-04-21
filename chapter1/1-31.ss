#lang sicp

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (* (term a) result))))
  (iter a 1))

(define (square x) (* x x))

(define (term k)
  (/ (* 2 k (+ (* 2 k) 2))
     (square (+ (* 2 k) 1))))

(define (next a) (+ a 1))

(define (factorial a b)
  (* 4
     (sum term a next b)))

(factorial 1.0 100.0)
(factorial 1.0 1000.0)
(factorial 1.0 10000.0)

(define (sum-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (sum-recursive term (next a) next b))))

(define (factorial-recursive a b)
  (* 4
     (sum-recursive term a next b)))

(newline)
(factorial-recursive 1.0 100.0)
(factorial-recursive 1.0 1000.0)
(factorial-recursive 1.0 10000.0)