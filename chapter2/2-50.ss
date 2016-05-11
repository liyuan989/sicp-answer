#lang sicp

(#%require sicp-pict)

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (cons (+ (xcor-vect v1) (xcor-vect v2))
        (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (cons (- (xcor-vect v1) (xcor-vect v2))
        (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (cons (* s (xcor-vect v))
        (* s (ycor-vect v))))

(define (make-fram origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (caddr f))

(define (make-segment x1 y1 x2 y2)
  (cons (make-vect x1 y1)
        (make-vect x2 y2)))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
