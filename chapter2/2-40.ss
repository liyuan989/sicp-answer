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

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else
         (filter predicate (cdr sequence)))))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate 1 (- i 1))))
           (enumerate 1 n)))

(display (unique-pairs 6))
(newline)

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime? x)
  (define (square val) (* val val))
  (define (find-divisor n test)
    (cond ((> (square test) n) n)
          ((= (remainder n test) 0) test)
          (else
           (find-divisor n (inc test)))))
  (define (find-small-divisor n)
    (find-divisor n 2))
  (= (find-small-divisor x) x))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(display (prime-sum-pairs 6))
