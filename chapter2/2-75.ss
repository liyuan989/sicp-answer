#lang racket

(define (square x) (* x x))

(define (real-part z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part z)
  (* (magnitude z) (sin (angle z))))

(define (magnitude z)
  (car z))

(define (angle z) (cdr z))

(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* x (cos y)))
          ((eq? op 'img-part) (* x (sin y)))
          ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          (else
           (error "UnKnown op -- MAKE-FROME-MAG-ANG" op))))
  dispatch)

(define (apply-generate op arg) (arg op))
