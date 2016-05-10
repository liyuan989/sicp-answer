#lang sicp

(#%require sicp-pict)

(define (split outer inner)
  (define (try painter n)
    (if (= n 0)
        painter
        (let ((smaller (try painter (- n 1))))
          (outer painter (inner smaller smaller)))))
  (lambda (painter n)
    (try painter n)))

(define right-split (split beside below))

(define up-split (split below beside))

(paint (right-split einstein 3))
(paint (up-split einstein 3))
