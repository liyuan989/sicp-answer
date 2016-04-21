#lang sicp

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))


(define (accumulate-iter combiner null-value term a next b)
  (if (> a b)
      null-value
      (accumulate-iter combiner (combiner (term a) null-value) term (next a) next b)))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))

(define (sum-int a b)
  (define (f a) a)
  (sum f a inc b))

(define (sum-int-iter a b)
  (define (f a) a)
  (sum-iter f a inc b))

(sum-int 1 100)
(sum-int-iter 1 100)