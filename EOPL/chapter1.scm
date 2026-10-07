#lang racket

;; A program can be defined by a procedures but does require some additional special forms
;; SOME speical forms

;; 1. define
;; 2. conditionals
;;
;; Exercise 1.2.1
(define x '(a b ((3) c) d))
(car (cdr x))
;; b
(caddr x)
;; ((3) c)
(cdaddr x)
;; c
(char? (car '(#\a #\b)))
;; #t
(cons 'x x)
;; ('x a b ((3) c) d)
(cons (list 1 2) (cons 3 '(4)))
;; ('(1 2) 3 4)
(cons (list) (list 1 (cons 2 '())))
;; ('() 1 (2))

;; eq? is very important in detecting the sharing!!
;; Exercise 1.2.2
;;
(define x1 '(a b))
(define x2 '(a))
(define x3 (cons x1 x2))

x1
;; '(a b)

;; (eq? x3 (cons x1 x2))
;; #f -> create a new pair

;; (eq? (cdr x3) x2)
;; #t

;; (eq? (car x1) (car x2))
;; #t
;;
;;(cons (cons 'a 'b) (cons 'c '()))
;; '((a . b) c)
;;
;;(cons 1 (cons 2 3))
;; ((1 2) . 3)

(define v1 (vector (cons 1 2) 3))
(define v2 (vector 'a v1))
v2
;; '# (a #((1 . 2) 3))

(define v3 '#(a #((1 . 2) 3)))
(eq? v1 v3)
;;  #f

(eq? v1 (vector-ref v2 1))
;; v1 ->   '#((1 . 2) 3)
;; v2 1 -> '#((1 . 2) 3)
;; #t

(eq? (vector-ref v1 0)
     (vector-ref (vector-ref v2 1) 0))
;; (vector-ref v1 0) -> (1. 2)
;; (vector-ref v2 1) -> #'((1 . 2) 3)
;; (vector-ref #'((1 . 2) 3) 0) -> (1 . 2)
;; #t


(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

(define add2
  (lambda (x)
    (+ x 2)))

;; Exercise 1.3.1
;; ((lambda (x)
;;   (list x (list (quote quote) x))))

;; this Creates a lambda function which takes an one arg and then creates
;; a list of '(x ('quote x)), where x is the argument passed.
;; this will fail to run since its run without an argument

(quote (lambda (x)
         (list x (list (quote quote) x))))
;; this is the same but this will run since its quoted.
;; it will be stored as a literal

;; 1.3.2

(define cell-tag "cell")

(define make-cell
  (lambda (x)
    (vector cell-tag x)))

(define cell?
  (lambda (x)
    (if (vector? x)
        (if (= (vector-length x) 2)
            (eq? (vector-ref x 0) cell-tag)
            #f)
        #f)))

(define cell-ref
  (lambda (x)
    (if (cell? x)
        (vector-ref x 1)
        (error "Invalid argument to cell-ref: " x))))

(define c (make-cell 5))
c
;; #'("cell" 5)

(cell? c)
;; #t

(cell-ref c)
;; 5

;; currying
;;
;; ((p' x1) x2 .... xn) = (p x1 ... xn)
;; (((p'' x1)x2) .... xn) = (px1 x2 ..... xn)

;; any function of n, where n >=2  arg can be converted to taking n - 1 arguments

;; Exe. 1.3.4
(define (curry2 fun)
  (lambda (x)
    (lambda (y)
      (apply fun (list x y)))))

;; 1.3.5

(define (compose-rr)
  (lambda (f)
    (lambda (g)
      (lambda (x)
        (f (g x))))))

;; 1.3.6

;; exercise 1.3.7
;;
(define compose-3
  (lambda x
    (if (or (<= (length x) 1) (> (length x) 3))
        (error "only accept 2 or 3 arg")
        (if (null? (cddr x))
            (compose (car x) (cadr x)) ;; only 2 elements
            (((compose-rr) (compose (cadr x) (caddr x))) (car x))))))
