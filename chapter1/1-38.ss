#lang sicp

(define (cont-frac n d k)
  (define (cal n d count)
    (if (= count k)
        (/ (n count) (d count))
        (/ (n count) (+ (d count)
                    (cal n d (+ count 1))))))
  (cal n d 1))

(define (cont-frac-iter n d k)
  (define (cal n d count result)
    (if (= count 0)
        result
        (cal n d (- count 1) (/ (n count) (+ (d count) result)))))
  (cal n d (- k 1) (/ (n k) (d k))))

(define (e k)
  (+ 2
     (cont-frac (lambda (x) 1.0)
                (lambda (x)
                  (if (= (remainder x 3) 2)
                      (* 2 (+ (quotient x 3) 1))
                      1.0))
                k)))

(define (e-iter k)
  (+ 2
     (cont-frac-iter (lambda (x) 1.0)
                     (lambda (x)
                       (if (= (remainder x 3) 2)
                           (* 2 (+ (quotient x 3) 1))
                           1.0))
                     k)))

(e 1000)
(e-iter 1000)