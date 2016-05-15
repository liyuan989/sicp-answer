#lang racket

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (encode-symbol s tree)
  (cond ((leaf? tree) '())
        ((symbol-in-tree? s (left-branch tree))
         (cons 0
               (encode-symbol s (left-branch tree))))
        ((symbol-in-tree? s (right-branch tree))
         (cons 1
               (encode-symbol s (right-branch tree))))
        (else (error "This symgol is not in tree: " s))))

(define (symbol-in-tree? s tree)
  (find s (symbols tree)))

(define (find x s)
  (cond ((null? s) #f)
        ((eq? x (car s)) #t)
        (else (find x (cdr s)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (cond ((null? set) '())
        ((= 1 (length set)) (car set))
        (else
         (let ((new-tree (make-code-tree (car set)
                                         (cadr set)))
               (remained-set (cddr set)))
           (successive-merge (adjoin-set new-tree remained-set))))))

(define tree
  (generate-huffman-tree '((NA 16) (YIP 9) (SHA 3) (A 2) (GET 2) (JOB 2) (BOOM 1) (WAH 1))))

(define msg-1 '(GET A JOB))
(encode msg-1 tree)
(length (encode msg-1 tree))

(define msg-2 '(SHA NA NA NA NA NA NA NA NA))
(encode msg-2 tree)
(length (encode msg-2 tree))

(define msg-3 '(WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP))
(encode msg-3 tree)
(length (encode msg-3 tree))

(define msg-4 '(SHA BOOM))
(encode msg-4 tree)
(length (encode msg-4 tree))
