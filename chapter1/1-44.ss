#lang sicp

(define dx 0.00001)

(define (square x) (* x x))

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx)))
       3)))

((smooth square) 5)

(define (repeated f n)
  (lambda (x)
    (define (recursive k)
      (if (= k n)
          (f x)
          (f (recursive (inc k)))))
    (recursive 1)))

(define (smooth-n f n)
  ((repeated smooth n) f))

((smooth-n square 10) 5) 