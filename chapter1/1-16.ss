#lang sicp

(define (fast-expt b n)
  (define (square n) (* n n))
  (define (even? n)
    (= (remainder n 2) 0))
  (define (fast-inner b n sum)
    (cond ((= n 0) sum)
          (else (if (even? n)
                    (fast-inner (square b)
                                (/ n 2)
                                sum)
                    (fast-inner b
                                (- n 1)
                                (* b sum))))))
  (fast-inner b n 1))

(fast-expt 2 8)
(fast-expt 2 16)
(fast-expt 2 32)
(fast-expt 2 10)