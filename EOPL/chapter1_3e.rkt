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
