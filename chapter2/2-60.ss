#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else
         (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else 
         (cons (car set1)
               (union-set (cdr set1) set2)))))

(define (intersection-set set1 set2)
  (define (iter s1 s2 result)
    (cond ((or (null? s1) (null? s2)) result)
          ((and (element-of-set? (car s1) s2)
                (not (element-of-set? (car s1) result)))
           (iter (cdr s1) s2 (cons (car s1) result)))
          (else
           (iter (cdr s1) s2 result))))
  (iter set1 set2 nil))

(intersection-set nil '(1 2 3))
(intersection-set (list 1 2 3) (list 1 2 3))
(intersection-set (list 1 2 1 2) (list 1 2 3))
