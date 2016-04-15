#lang sicp

(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))

(f 10)
(f 3)

(define (g n)
  (define (inner-g a b c count)
    (cond ((= count 0) c)
          (else (inner-g b
                         c
                         (+ (* 3 a)
                            (* 2 b)
                            c)
                         (- count 1)))))
  (cond ((< n 3) n)
        (else (inner-g 0 1 2 (- n 2)))))

(g 10)
(g 3)
