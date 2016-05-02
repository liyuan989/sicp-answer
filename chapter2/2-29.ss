#lang sicp

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (define (branch-weight b)
    (if (pair? b)
        (let ((structure (branch-structure b)))
          (cond ((null? structure)
                 0)
                ((pair? structure)
                 (total-weight structure))
                (else
                 structure)))
        b))
  (cond ((null? mobile) 0)
        ((pair? mobile)
         (+ (branch-weight (left-branch mobile))
            (branch-weight (right-branch mobile))))
        (else mobile)))

(define (is-balance? mobile)
  (if (null? mobile)
      #t
      (let ((left (left-branch mobile))
            (right (right-branch mobile)))
        (= (* (branch-length left)
              (if (null? left)
                  0
                  (total-weight (branch-structure left))))
           (* (branch-length right)
              (if (null? right)
                  0
                  (total-weight (branch-structure right))))))))

(is-balance? (make-mobile (make-branch 2 3)
                          (make-branch 3 2)))

(is-balance? (make-mobile (make-branch 2 3)
                          (make-branch 3 4)))