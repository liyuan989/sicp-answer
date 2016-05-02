#lang sicp

(define (same-parity f . table)
  (let ((parity (remainder f 2)))
    (define (inner t)
      (cond ((null? t)
              '())
            ((= (remainder (car t) 2) parity)
             (cons (car t) (inner (cdr t))))
            (else
             (inner (cdr t)))))
    (cons f (inner table))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
