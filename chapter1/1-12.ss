#lang sicp

(define (pascal row column)
  (cond ((or (= column 0) (= column row)) 1)
        (else (+ (pascal (- row 1) (- column 1))
                 (pascal (- row 1) column)))))

(pascal 0 0)
(pascal 2 1)
(pascal 4 2)

; (row,column) = row! / (column! * (row - column)!)
(define (pascal-iter row column)
  (define (factorial n)
    (define (fact-iter sum count)
      (if (= count 0)
          sum
          (fact-iter (* sum count) (- count 1))))
    (fact-iter 1 n))
  (/ (factorial row)
     (* (factorial column)
        (factorial (- row column)))))

(pascal-iter 0 0)
(pascal-iter 2 1)
(pascal-iter 4 2)
(pascal-iter 1000 999)