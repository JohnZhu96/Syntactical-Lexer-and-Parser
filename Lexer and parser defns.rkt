#lang racket

;
; *** STRING PRELIMINARIES ***
;

; EXERCISE 0


(define (string-into-list s)
  (define (convert i result)
    (if (= i (string-length s))
        (reverse result)
        (convert (+ i 1) (cons (string-ref s i) result))))
  (convert 0 '()))

; EXERCISE 1: removing white space

(define (space? c)
  (equal? c #\space))

(define (remove-spaces lc)
  (if (space? (car lc))
      (remove-spaces (cdr lc))
      (write lc)))  ; lc for "list of characters"


; EXERCISE 2: digits and turning digits into numbers

(define (digit? c)
  (or (equal? c #\1)
      (equal? c #\2)
      (equal? c #\3)
      (equal? c #\4)
      (equal? c #\5)
      (equal? c #\6)
      (equal? c #\7)
      (equal? c #\8)
      (equal? c #\9)
      (equal? c #\0)))

(define (digit->number c)
  (cond ((equal? c #\1) 1)
        ((equal? c #\2) 2)
        ((equal? c #\3) 3)
        ((equal? c #\4) 4)
        ((equal? c #\5) 5)
        ((equal? c #\6) 6)
        ((equal? c #\7) 7)
        ((equal? c #\8) 8)
        ((equal? c #\9) 9)
        ((equal? c #\0) 0)))

(define (number list-of-digits)
  (define (list-length list-of-digits)
    (if (null? list-of-digits)
        0
        (+ 1 (list-length (cdr list-of-digits)))))
  (define (exp base n)
    (if (= n 0)
        1
        (* base (exp base (- n 1)))))
  (define (convert list-of-digits int)
    (if (null? list-of-digits)
        int
        (convert (cdr list-of-digits) (+ (* (digit->number (car list-of-digits))
                                            (exp 10 (- (list-length list-of-digits) 1))) int))))
  (convert list-of-digits 0))


(define (list-of-digits? lc)
;  (write lc) (newline)
  (if (null? lc)
      #t
      (and (digit? (car lc))
           (list-of-digits? (cdr lc)))))

;
; *** LEXICAL ANALYSIS ***
;

; EXERCISE 3

(define (token? char)
  (cond ((space? char) #t)
        ((equal? char #\() #t)
        ((equal? char #\)) #t)
        (else #f)))

(define (lexer acc lc)
  (define (helper acc lc result)
    (if (null? lc)
        (if (null? acc)
            (reverse result)
            (cons (reverse acc) '()))
        (if (token? (car lc))
            (if (null? acc)
                (if (equal? (car lc) #\space)
                    (helper acc (cdr lc) result)
                    (helper acc (cdr lc) (cons (car lc) result)))                
                (if (equal? (car lc) #\space)
                    (helper '() (cdr lc) (cons (reverse acc) result))
                    (helper '() (cdr lc) (cons (car lc) (cons (reverse acc) result)))))
            (helper (cons (car lc) acc) (cdr lc) result))))
 (if (equal? (car lc) #\")
     lc
     (helper acc lc '())))


; (this might help coding the above procedure... up to you, but it was part of my code...)

(define (lexer-string acc lc)
  (if (equal? #\" (car lc))
      (cons (reverse (cons (car lc) acc)) (lexer '() (cdr lc)))
      (lexer-string (cons (car lc) acc) (cdr lc))))

; EXERCISE 4
(define (delete-quotes lst)
  (cond ((null? lst) '())
        ((char? (car lst)) 
         (let ((ch (car lst)))
           (if (equal? ch #\")
               (delete-quotes (cdr lst))
               (cons ch (delete-quotes (cdr lst)))))) 
        (else (cons (car lst) (delete-quotes (cdr lst)))))) 

(define (symbolize s)
  (define (is-digit-list? s)
    (if (null? s)
        #t
        (if (digit? (car s))
            (is-digit-list? (cdr s))
            #f)))
  (define (is-symbol-list? s)
    (if (null? s)
        #t
        (if (string? (car s))
            (is-symbol-list? (cdr s))
            #f)))
   (cond ((is-digit-list? s) (list 'NUMBER (number s)))
      ((equal? (car s) #\")(list 'STRING (apply string (delete-quotes s))))
      ((equal? s #\() s)
      ((equal? s #\)) s)
      (else (list 'SYMBOL (apply string s)))))

; EXERCISE 5

(define (lex lc)
  (define (lex-helper lc result)
    (if (null? lc)
        (reverse result)
        (if (pair? (car lc))
            (lex-helper (cdr lc) (cons (symbolize (car lc)) result))
            (lex-helper (cdr lc) (cons (car lc) result)))))
  (lex-helper (lexer '() (string-into-list lc)) '()))

(lex "(apple boy 123 123x)")

(lex "(define (square x) (* x x 5 56))")

(define foo (lex "(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))"))

(define goo (lex "(define (square x) (* \"times of\" x x 25))"))

;
; *** PARSER ***
;

; these procedures might help...

(define (begins? token tag)
  (equal? (car token) tag))

(define (translate token)
  (cond ((begins? token 'SYMBOL) (string->symbol (cadr token)))
        ((begins? token 'NUMBER) (cadr token))
        ((begins? token 'STRING) (cadr token))))

(define (pop acc s)
  (if (equal? (car s) #\()
      (cons acc (cdr s))
      (pop (cons (car s) acc) (cdr s))))

; EXERCISE 6

(define (parser s tokens)
  (write s) (newline) (write tokens) (newline) (newline)
  (if (null? tokens)
      s
      (if (equal? (car tokens) #\))
          (parser (pop '() s) (cdr tokens))
          (if (equal? (car tokens) #\()
              (parser (cons (car tokens) s) (cdr tokens))
              (parser (cons (translate (car tokens)) s) (cdr tokens))))))

; EXERCISE 7

(define (parse tokens) (car (parser '() tokens)))
