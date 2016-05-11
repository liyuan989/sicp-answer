#lang sicp

(define (below painter1 painter2)
  (let ((down (transform-painter painter1
                               (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               (make-vect 0.0 0.5)))
        (up (transform-painter painter2
                                 (make-vect 0.0 0.5)
                                 (make-vect 1.0 0.5)
                                 (make-vect 0.0 1.0))))
    (lambda (frame)
      (up frame)
      (down frame))))

(define (below2 painter1 painter2)
  (let ((m (beside (rotate270 painter1) (rotate 270 painter2))))
    (rotate270 m)))
