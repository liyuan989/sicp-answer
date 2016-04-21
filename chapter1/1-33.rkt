#lang sicp

(define (square x) (* x x))

(define (smallest-divisor n test)
  (cond ((> (square test) n) n)
        ((= (remainder n test) 0) test)
        (else (smallest-divisor n (+ test 1)))))

(define (prime? n)
  (= (smallest-divisor n 2) n))

(define (accumulate combiner null-value term a next b predicate)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a b predicate) next b predicate))))


(define (accumulate-iter combiner null-value term a next b predicate)
  (if (> a b)
      null-value
      (accumulate-iter combiner (combiner (term a) null-value) term (next a b predicate) next b predicate)))

(define (identity x) x)

(define (next-sum a b predicate)
  (if (predicate (+ a 1))
      (+ a 1)
      (next-sum (+ a 1) b predicate)))

(define (sum-prime a b)
  (accumulate + 0 identity a next-sum b prime?))

(define (sum-prime-iter a b)
  (accumulate-iter + 0 identity a next-sum b prime?))

(sum-prime 1 10)
(sum-prime-iter 1 10)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (each-gcd? a b)
  (= (gcd a b) 1))

(define (product-next a b predicate)
    (if (predicate (+ a 1) b)
        (+ a 1)
        (product-next (+ a 1) b predicate)))

(define (product-each-prime a b)
  (accumulate * 1 identity a product-next b each-gcd?))

(define (product-each-prime-iter a b)
  (accumulate-iter * 1 identity a product-next b each-gcd?))

(product-each-prime 1 10)
(product-each-prime-iter 1 10)