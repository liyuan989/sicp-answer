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

(define (golden-ratio k)
 (+ 1 (cont-frac (lambda (i) 1.0)
                 (lambda (i) 1.0)
                 k)))

(define (golden-ratio-iter k)
  (+ 1 (cont-frac-iter (lambda (i) 1.0)
                       (lambda (i) 1.0)
                       k)))

(golden-ratio 1000)
(golden-ratio-iter 1000)
