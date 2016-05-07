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

(define (adjoin-position new-row k reset-of-queens)
  (cons new-row reset-of-queens))

(define (safe? k positions)
  (define (check row reset i)
    (if (null? reset)
        #t
        (let ((row-next (car reset)))
          (cond ((= row row-next) #f)
                ((= (+ row i) row-next) #f)
                ((= (- row i) row-next) #f)
                (else (check row (cdr reset) (+ i 1)))))))
  (check (car positions) (cdr positions) 1))

(define empty-board nil)

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions)
                  (safe? k positions))
                (flatmap (lambda (reset-of-queens)
                           (map (lambda (new-row)
                                  (adjoin-position new-row k reset-of-queens))
                                (enumerate 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))

(display (queens 8))
