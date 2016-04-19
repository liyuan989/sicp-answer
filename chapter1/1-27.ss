#lang sicp

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((= (remainder exp 2) 0)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))


(define (check? a n)
  (cond ((= a 0) #t)
        ((= (expmod a n n) a)
         (check? (- a 1) n))
        (else #f)))
  

(define (check-carmichael n)
  (newline)
  (cond ((check? (- n 1) n)
         (display "yes!"))
        (else
         (display "no!"))))

(check-carmichael 561)
(check-carmichael 1105)
(check-carmichael 1729)
(check-carmichael 2465)
(check-carmichael 2821)
(check-carmichael 6601)
