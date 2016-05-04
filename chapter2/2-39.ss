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

(define (reverse-seq seq)
  (fold-right (lambda (x y)
                (if (null? y)
                    (cons x nil)
                    (append y (cons x nil))))
              nil
              seq))

(define (reverse-seq2 seq)
  (fold-left (lambda (x y)
               (cons y x))
             nil
             seq))

(display (reverse-seq (list 1 2 3 4)))
(newline)
(display (reverse-seq2 (list 1 2 3 4)))
