#lang sicp

(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (no-more? coins) (null? coins))

(define (first-denomination coins) (car coins))

(define (except-first-denomination coins) (cdr coins))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(cc 100 us-coins)
(cc 100 (list 1 5 10 25 50))
(cc 100 uk-coins)
(cc 100 (list 0.5 1 2 5 10 20 50 100))