#lang sicp

(define (fold-right op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (fold-right op init (cdr sequence)))))

(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))

(fold-right / 1 (list 1 2 3))

(fold-left / 1 (list 1 2 3))

(display (fold-right list nil (list 1 2 3)))
(newline)
(display (fold-left list nil (list 1 2 3)))
