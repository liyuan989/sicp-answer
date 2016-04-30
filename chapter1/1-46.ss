#lang sicp

(define (iterative-improve close?)
  (lambda (x f)
    (define (try val)
      (if (close? val (f x val))
          (f x val)
          (try (f x val))))
    (try 1.0)))

(define sqrt
  (iterative-improve (lambda (y x)
                       (< (abs (- y x)) 0.00001))))

(sqrt 4 (lambda (y x)
          (/ (+ x (/ y x)) 2)))

(define fixed-point
  (iterative-improve (lambda (y x)
                       (< (abs (- y x)) 0.00001))))

(fixed-point 1.0 (lambda (y x)
                   (+ (sin x) (cos x))))