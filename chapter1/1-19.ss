#lang sicp

; a <- bq + a(p + q)
; b <- bp + aq

; a <- b(2pq + q^2) + a(p^2 + q^2 + 2pq + q^2)
; b <- b(p^2 + q^2) + a(2pq + q^2)

; p' = p^2 + q^2
; q' = 2pq + q^2

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((= (remainder count 2) 0)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q) (* q q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(fib 0)
(fib 1)
(fib 2)
(fib 5)
(fib 10)
(fib 100)