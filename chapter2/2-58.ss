#lang sicp

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))

(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else
         (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else
         (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '^)))

(define (base e) (car e))

(define (exponent e) (caddr e))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else
         (list '^ base exponent))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product
                        (make-exponentiation (base exp)
                                             (- (exponent exp) 1))
                        (deriv (base exp) var))))
        (else
         (display "unknown expression type -- DERIV ")
         (display exp))))

(deriv '(x + 3) 'x)
(deriv '(x + y) 'x)
(display (deriv '((x * y) * (x * 3)) 'x))
(newline)
(display (deriv '(x ^ 3) 'x))
