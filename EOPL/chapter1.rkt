#lang eopl

; In-S? : N -> Bool
(define in-S?
  (lambda (n)
    (if (zero? n) #t
        (if (>= (- n 3) 0)
            (in-S? (- n 3))
            #f))))

; nth-element: List x Int -> Int
; usage: nth element in the list
(define nth-element
  (lambda (lst n)
    (if (null? lst)
        (report-list-null-error n)
        (if (zero? n)
            (car lst)
            (nth-element (cdr lst) (- n 1))))))

(define report-list-null-error
  (lambda (n)
    (eopl:error 'nth-element "List too short by ~s elements ~%" (+ n 1))))

; Exercise 1.6 [*]
; If the order of the check is reversed, then we will get an error with car saying pair? expected given ()

; Exercise 1.7 [**]
; looking at this, we need to store the original number and the list. so we can
; define some kind of closure which captures this.

(define better-report-list-error
  (lambda (lst n)
    (lambda ()
      (eopl:error 'nth-element "~s does not have ~s elements ~%" lst n))))

(define nth-element-helper
  (lambda (lst n error-proc)
    (if (null? lst)
        (error-proc)
        (if (zero? n)
            (car lst)
            (nth-element (cdr lst) (- n 1))))))

(define nth-element-better
  (lambda (lst n)
    (nth-element-helper lst n (better-report-list-error lst n))))

; Exercise 1.11 [*]
;;  S-list ::= ()
;;         ::= (S-exp . S-list)
;;  S-exp  ::= Symbol | S-list
;;
;; Even when the recursion of subset-in-s-exp is on sexp, it works since "sexp" basically in
;;  S-exp  ::= S-list. This is smaller than the original s-list since its "contained inside it".
;;  This is guarenteed to halt.


; Exercise 1.22

; filter-in :: (a -> Boolean) X ListOf(a) -> ListOf(a)
(define filter-in
  (lambda (pred lst)
    (if (null? lst)
      lst
      (if (pred (car lst))
        (cons (car lst) (filter-in pred (cdr lst)))
        (filter-in pred (cdr lst))))))

;; tests               
(filter-in number? '(a 2 (1 3) b 7))

; Exercise 1.23

;list-index-helper :: (a -> Boolean) -> ListOf(a) -> Int -> Int
(define list-index-helper
  (lambda (pred lst n)
    (if (null? lst)
      #f
      (if (pred (car lst))
        n
        (list-index-helper pred (cdr lst) (+ n 1))))))

; list-index:: (a -> Boolean) -> ListOf(a) -> Int
(define list-index
  (lambda (pred lst)
    (list-index-helper pred lst 0)))


; Exercise 1.24

(define (every? pred lst)
  (if (null? lst)
    #t
    (if (not (pred (car lst)))
      #f
      (every? pred (cdr lst)))))


; Exercise 1.25
; exists? :: (a -> Bool) -> ListOf (a) -> Bool
; usages? :: any element satistifes

(define (exists? pred lst)
  (if (null? lst)
    #f
    (if (pred (car lst))
      #t
      (exists? pred (cdr lst)))))


; Exercise 1.26 

; acc-to-list :: ListOf (a) -> ListOf(a) -> ListOf (a)
; usage? :: take two list and add one to another

(define (acc-to-lst lst1 lst2)
  (if (null? lst1)
    lst2
    (if (null? lst2)
      lst1
      (cons (car lst1) 
            (acc-to-lst (cdr lst1) lst2)))))

; up-gen :: ListOf (a) -> ListOf (a) -> ListOf (a)
; usage? :: remove the parent from top level and add to another list
(define (up-gen lst acc)
  (if (null? lst)
    acc
    (if (list? (car lst))
      (up-gen (cdr lst) (acc-to-lst acc (car lst)))
      (up-gen (cdr lst) (acc-to-lst acc (list (car lst)))))))


; up :: ListOf (a) -> ListOf (a)
; usage? :: remove the paren from top level elemnets
(define (up lst)
  (up-gen lst '()))

;; Exercise 1.27
;; flatten :: slist -> slist
;; usage?::flattern slist

(define (flatten slst)
  (if (null? slst)
    '()
    (acc-to-lst (flatten-sexp (car slst))
                (flatten (cdr slst)))))

; flatten-sexp :: sexp -> slist              
(define (flatten-sexp sexp)
  (if (symbol? sexp)
    (list sexp)
    (flatten sexp)))

; As can be seen, mutual recursion makes wirting function so
; easy


;; Exercise 1.31

; leaf :: Int -> Int
