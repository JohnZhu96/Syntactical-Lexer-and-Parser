#lang Racket

(define I (lambda(v) v))
(define (square n) (* n n))
(define (double n) (* 2 n))


;function that returns the same value as member? using continuation
(define (Member? x L k)
  (if (null? L)
      (k #f)
      (if (equal? x (car L))
          (k #t)
          (Member? x (cdr L) (lambda(v) (k v))))))

; Fastexp using continuation
(define (Fastexp b e k)
  (if (= e 0)
      (k 1)
      (if (even? e)
          (Fastexp b (/ e 2) (lambda(v) (k (square v))))
          (Fastexp b (- e 1) (lambda(v) (k (* b v)))))))

;Fastmult using continuation
(define (Fastmult m n k)
  (if (= n 0)
      (k 0)
      (if (even? n)
          (Fastmult m (/ n 2) (lambda(v) (k (double v))))
          (Fastmult m (- n 1) (lambda(v) (k (+ m v)))))))

;Map using continuation
(define (Map f L k)
  (if (null? L)
      (k '())
      (Map f (cdr L) (lambda(v) (k (cons (f (car L)) v))))))

;Filter using continuation
(define (Filter pred L k)
  (if (null? L)
      (k '())
      (if (pred (car L))
          (Filter pred (cdr L) (lambda(v) (k (cons (car L) v))))
          (Filter pred (cdr L) (lambda(v) (k v))))))

;Tack using continuation
(define (Tack x L k)
  (if (null? L)
      (k (cons x '()))
      (Tack x (cdr L) (lambda(v) (k (cons (car L) v))))))

;Reverse using continuation
(define (Reverse L k)
  (if (null? L)
      (k '())
      (Reverse (cdr L) (lambda (v) (k (Tack (car L) v I))))))

;Append using continuation
(define (Append S T k)
  (if (null? S)
      (k T)
      (Append (cdr S) T (lambda (v) (k (cons (car S) v))))))

;Fib using continuation
(define (Fib n k)
  (if (< n 2)
      (k n)
      (Fib (- n 1) (lambda (v) (Fib (- n 2) (lambda (u) (k (+ v u))))))))

;Fringe using continuation
(define (Fringe S k)
  (if (null? S)
      (k '())
      (if (number? S)
          (k (list S))
          (Fringe (car S)
                  (lambda (v) (Fringe (cdr S)
                                      (lambda (u) (k (Append v u I)))))))))

;Tag using continuation
(define (Tag x L k)
  (if (null? L)
      (k '())
      (Tag x (cdr L) (lambda (v) (k (cons (cons x (car L)) v))))))

;Powerset using continuation
(define (Powerset S k)
  (if (null? S)
      (k '(()))
      (Powerset (cdr S)
                (lambda(v) (k ((lambda (T) (Append (Tag (car S)T I) T I))
                              v))))))

;Cross using continuation
(define (Cross S T k)
  (if (null? S)
      (k '())
      (Cross (cdr S) T
             (lambda (v) (k (Append (Tag (car S) T I) v I))))))

;Largers using continuation
(define (Largers x L k)
  (Filter (lambda (n) (>= n x)) L k))

;Smallers using continuation
(define (Smallers x L k)
  (Filter (lambda (n) (< n x)) L k))

;Quicksort using continuation
(define (Quicksort F k)
  (if (null? F)
      (k '())
      (Quicksort (Smallers (car F) (cdr F) I)
                 (lambda (v) (k (Quicksort (Largers (car F) (cdr F)I)
                                           (lambda (u) (Append v (cons (car F) u)I))))))))

