#lang sicp

(define (repeated f n)
  (lambda (x)
    (define (recursive k)
      (if (= k n)
          (f x)
          (f (recursive (inc k)))))
    (recursive 1)))

(define tolerance 0.00001)

(define (square x) (* x x))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try-it guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try-it next))))
  (try-it first-guess))

(define (average-damp f)
  (lambda (x)
    (/ (+ x (f x)) 2)))

(define (expt base n)
  (if (= n 0)
      1
      ((repeated (lambda (x) (* base x)) n) 1)))

(define (average-damp-n-times f n)
  ((repeated average-damp n) f))

;((average-damp-n-times square 100) 10.0)

(define (damped-nth-root n damp-times)
  (lambda (x)
    (fixed-point (average-damp-n-times
                  (lambda (y)
                    (/ x (expt y (- n 1))))
                  damp-times)
                 1.0)))

(define (log2 x)
  (floor (/ (log x)
            (log 2))))

(define (nth-root n)
  (damped-nth-root n (log2 n)))

(define sqrt (nth-root 2))
(sqrt 4)

(define cube (nth-root 3))
(cube 8)

(define 4th-root (nth-root 4))
(4th-root 16)

(define 16th-root (nth-root 16))
(16th-root 65536)

