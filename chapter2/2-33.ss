#lang sicp

(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (accumulate op init (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              nil sequence))

(display (map (lambda (x) (* x x))
              (list 1 2 3 4)))
(newline)

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(display (append (list 1 2 3) (list 4 5 6)))
(newline)

(define (length sequence)
  (accumulate (lambda (x y)
                (+ 1 y))
              0
              sequence))

(display (length (list 1 2 3 4 5)))
