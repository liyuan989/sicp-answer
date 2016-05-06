#lang sicp

(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (accumulate op init (cdr sequence)))))

(define (enumerate first last)
  (define (iter a b)
    (cond ((> a b) nil)
          ((= a b)
           (cons a nil))
          (else (cons a
                      (iter (+ a 1) b)))))
  (iter first last))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate 1 n)))
           (enumerate 1 n)))

(define (triples n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (cons i j))
                  (pairs n)))
           (enumerate 1 n)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else
         (filter predicate (cdr sequence)))))

(define (three-num n s)
  (filter (lambda (seq)
            (cond ((null? seq)
                   #f)
                  ((or (= (car seq) (cadr seq))
                       (= (car seq) (caddr seq))
                       (= (cadr seq) (caddr seq)))
                   #f)
                  ((= (+ (car seq)
                         (cadr seq)
                         (caddr seq))
                      s)
                   #t)
                  (else #f)))
          (triples n)))

(display (three-num 6 6))
