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

(define (tan-cf x k)
  (cont-frac (lambda (a)
               (if (= a 1)
                   x
                   (- (* x x))))
             (lambda (a) (- (* 2 a) 1))
             k))

(define (tan-cf-iter x k)
  (cont-frac-iter (lambda (a)
               (if (= a 1)
                   x
                   (- (* x x))))
             (lambda (a) (- (* 2 a) 1))
             k))

(tan-cf 10.0 100)
(tan-cf-iter 10.0 100)