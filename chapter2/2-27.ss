#lang sicp

(reverse (list (list 1 2) (list 3 4)))

(define (reverse-deep t)
  (if (null? (cdr t))
      (if (pair? (car t))
          (reverse (car t))
          (car t))
      (cons (reverse-deep (cdr t))
            (if (pair? (car t))
                (reverse (car t))
                (car t)))))

(reverse-deep (list (list 1 2) (list 3 4)))