#lang sicp

(define (next-test-divisor n)
  (if (= (remainder n 2) 0)
      (+ n 1)
      (+ n 2)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (square a) (* a a))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-test-divisor n)))))

(define (divides? a b)
  (= (remainder b a) 0))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

(define (prime? n)
  (= (smallest-divisor n) n))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (next-odd n) (next-test-divisor n))

(define (continue-primes n count)
  (cond ((= count 0) (display "complete!"))
        ((prime? n)
         (display n)
         (newline)
         (continue-primes (next-odd n) (- count 1)))
        (else (continue-primes (next-odd n) count))))

(define (search-for-primes n)
  (let ((start-time (runtime)))
    (continue-primes n 3)
    (newline)
    (- (runtime) start-time)))

(search-for-primes 1000)
(search-for-primes 10000)
(search-for-primes 100000)
(search-for-primes 1000000)
