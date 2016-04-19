#lang sicp

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((nontrivial-square-root? base m)
         0)
        ((= (remainder exp 2) 0)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (nontrivial-square-root? a n)
  (and (not (= a 1))
       (not (= a (- n 1)))
       (= (remainder (square a) n)
          1)))

(define (miller-check n)
  (define (inner-check? a n)
    (cond ((= a 0) #t)
          ((= (expmod a (- n 1) n)
              (remainder 1 n))
           (inner-check? (- a 1) n))
          (else #f)))
  (newline)
  (cond ((inner-check? (- n 1) n)
         (display "yes!"))
        (else
         (display "no!"))))

(miller-check 561)
(miller-check 1105)
(miller-check 1729)
(miller-check 2465)
(miller-check 2821)
(miller-check 6601)