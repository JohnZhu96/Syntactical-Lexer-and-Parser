#lang racket

; Stream preliminaries

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b) (cons a (delay b)))))

(define head car)

(define (tail s) (force (cdr s))) (define stream-car car)

(define stream-cdr tail)

(define the-empty-stream (delay '()))

(define (empty-stream? s)
 (and (not (pair? s))
      (null? (force s))))

(define (map-stream fn S)
  (cons-stream (fn (head S))
              (map-stream fn (tail S))))

(define (filter-stream pred S)
  (if (pred (head S))
      (cons-stream (head S) (filter-stream pred (tail S)))
      (filter-stream pred (tail S))))

(define (1+ x) (+ 1 x))

(define (take n G)
  (if (= n 0)
      '()
      (cons (head G) (take (- n 1) (tail G)))))

(define (drop n G)
  (if (= n 0)
      G
      (drop (- n 1) (tail G))))

(define zeros (cons-stream 0 zeros))

(define (ints n) (cons-stream n (ints (+ n 1))))

(define integers (ints 0))

;function that calculates square of a series
(define (square G) (prod G G))

; Display power series

;Problem 0
;procuces an infinite power series 
(define (series l)
  (if (null? l)
      (cons-stream 0 (series '(0)))
      (cons-stream (car l) (series (cdr l)))))

(define (show n p)
  (define (showit i p)
    (if (> i n)
        '()
        (cons (if (= i 0)
                  (head p)
                  (list '* (head p) 'z^ i))
              (showit (+ i 1) (tail p)))))
  (cons '+ (showit 0 p)))

;Problem 1
;function that adds together two generating functions
(define (sum f g)
  (map-stream (lambda(x) (+ (car x) (cdr x))) (zip-streams f g)))

;star product
(define (star f g)
  (map-stream (lambda(x) (* (car x) (cdr x))) (zip-streams f g)))


;Problem 2
;zip-streams
(define (zip-streams f g)
  (cons-stream (cons (head f) (head g)) (zip-streams (tail f) (tail g))))


;Problem 3
;function that scales the coefficients of a generating function by a constant c
(define (scale c S)
  (cons-stream (* (head S) c) (scale c (tail S))))


;Problem 4
;function that multiplies a generating function by z 
(define (prod-z S)
  (cons-stream 0 S))

;Problem 5
;function that takes the derivative with respect to z 
(define (deriv S)
  (star (tail S) (ints 1)))

;Problem 6
;a function that calculates the product two power series
(define (prod G H)
  (cons-stream (* (head G) (head H)) (tail (sum (prod-z (sum (scale (head H) (tail G))
                                                              (scale (head G) (tail H))))
                                           (prod-z (prod-z (prod (tail G) (tail H))))))))

;Problem 7
;function that divides two functions
(define (divide G H)
  (define (subtract f g)
    (map-stream (lambda(x) (- (car x) (cdr x))) (zip-streams f g)))
  (cons-stream (/ (head G) (head H)) (divide  (subtract (tail G)
                                                        (scale (/ (head G) (head H)) (tail H)))
                                               H)))

;function that calculates the reciprocal
(define (reciprocal S)
  (divide (series '(1 0)) S))


;Problem 8
;function that retrieves the nth coefficient
(define (coeff n S)
  (if (null? S)
      0
      (if (= n 0)
          (head S)
          (coeff (- n 1) (tail S)))))


;Problem 9
;function that will raise a power series to an integer power
(define (expt S n)
  (cond ((= n 0) (series '(1 0)))
        ((= n 1) S)
        (else (prod S (expt S (- n 1))))))

;function that generates the pascal's triangle
(define (pascal n)
  (take (+ n 1) (expt (series '(1 1)) n)))

;function that computes the binomial coefficients
(define (binomial n k)
  (coeff k (pascal n)))


;Problem 10
(define (hat G)
  (prod G (reciprocal (series '(1 -1)))))
 
; For problem 11

(define golden-mean (/ (+ 1 (sqrt 5)) 2))

