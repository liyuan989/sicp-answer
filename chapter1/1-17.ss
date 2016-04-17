#lang sicp

(define (mul a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (double x) (* x 2))

(define (halve x) (/ x 2))

(define (fast-expt b n)
  (define (inner b n sum)
    (cond ((= n 0) sum)
          ((= (remainder n 2) 0) (inner (mul b b) (/ n 2) sum))
          (else (inner b (- n 1) (* b sum)))))
  (inner b n 1))

(fast-expt 2 4)
(fast-expt 2 8)
(fast-expt 2 10)
(fast-expt 2 16)
(fast-expt 2 32)
(fast-expt 2 64)