; inductive definition of list is

;  <list-of-numbers> ::= ()
; <list-of-numbers> ::= (<number> . <list-of-numbers>)


; Exercise 2.1.1
; ; --------------
; (-7 . (3 . (14 . () )))

;    <list-of-num>
; => (<number> . <list-of-numbers>)
; => (-7 . <list-of-numbers>)
; => (-7 . (<number> . <list-of-numbers>))
; => (-7 . (3 . <list-of-numbers>))
; => (-7 . (3 . (<nums> . <lon>)))
; => (-7 . (3 . (14 . (<lon>))))
; => (-7 . (3 . (14 . (() ))))


; Exercise 2.1.2
; --------------
; <datum-repeat> ::= <datum> | <datum> <datum-repeat>
;        <list>  ::= () | (<datum-repeat>)
; <dotted-datum> ::= (<datum-repeat> . <datum>)
; <vector>       ::= #() | #(<datum-repeat>)
; <datum>        ::= <number> | <symbol> | <boolean> | <string>
;                    | <list> | <dotted-datum> | <vector>


; (#t (foo . ()) 3)

; <list>
; => (<datum-repeat>) *
; => (<datum> <datum-repeat>) *
; => (<datum> <datum> <datum-repeat>) *
; (<datum> <datum> <datum>) *
; (<boolean> <datum> <datum>)
; (#t <datum> <datum>)
; (#t <dotted-datum> <datum>)
; (#t (<datum-repeat> . <datum>) <datum>) *
; (#t (<datum> . <datum>) <datum>) *
; (#t (<symbol> . <datum>) <datum>) *
; (#t (foo . <datum>) <datum>) *
; (#t (foo . <list>) <datum>)
; (#t (foo . ()) <datum>)
; (#t (foo . ()) <number>)
; (#t (foo . ()) 3)

; "*" indicates the changes to the original. We needed more derivation
; if we did not use kleen plus and star since we had to introduce <datum-repeat> which esentially does the same thing
; as kleen-star, which is repeat but it does it in more steps.

(define list-of-numbers?
  (lambda (lst)
    (if (null? lst)
        #t
        (if (pair? lst)
            (if (number? (car lst))
                (list-of-numbers? (cdr lst))
                #f)
            #f))))

; Proving the above code

; 1. taking (), we return #t since the only list with length 0 is null list.

; 2. assume list-of-numbers? works on length k. we need to show it works for k+1.

; let such list be lst

; lst has length > 0 thus according to the definition its created with
; 
; cons of number and a another-list
; 
; the (cdr lst) is lst' which has a length of k.
; 
; Accoring to the inductive hypothesis, list-of-numbers? works on lst'.
; 
; And accoring to function, car lst is a number and cdr lst is also list-of-number.
; 
; hence, lst is a list-of-number.
; 
; thus, this function is correct.

              (define nth-elt
                (lambda (lst n)
                  (if (null? lst)
                      (error "nth-elt : list too short")
                     (if (zero? n)
                     (car lst)
                     (nth-elt (cdr lst) (- n 1))))))

            (define list-length
              (lambda (lst)
                (if (null? lst)
                    0
                    (+ 1 (list-length (cdr lst))))))

; Exercise 2.2.1
; 
; both will throw error. nth-elt on zero? which does expect list and list-length on cdr.
; 
; list-ref and length also throws error but the context matters. nth-elt and list-legnth are exposing the inner working of the library when reporting error while
; the other two function does not.

(define nth-elt-2
  (lambda (lst n)
    (if (not (pair? lst))
        (error "nth-elt-2 : not a pair")
        (if (null? lst)
            (error "nth-elt : list too short")
            (if (zero? n)
                (car lst)
                (nth-elt (cdr lst) (- n 1)))))))


(define list-length-2
  (lambda (lst)
    (if (not (list? lst))
        (error "list-length-2 : not a list")
        (if (null? lst)
            0
            (+ 1 (list-length (cdr lst)))))))

;  <s-list> ::= (<{symbol-expression}*>)
;  <symbol-expression> := <symbol> | <s-list>

(define subst
  (lambda (new old slst)
    (if (null? slst)
        '()
        (cons (subst-symbol-expression new old (car slst))
              (subst new old (cdr slst))))))


(define subst-symbol-expression
  (lambda (new old se)
    (if (symbol? se)
        (if (eq? se old) new se)
        (subst new old se))))

; Exercise 2.2.4
; (subst new old se) is found when the car of the list is not a symbol but another s-list
; and we know this will halt eventually since its following BNF definition. car of a list will always be smaller than the list.

; Exercise 2.2.5
; Write subst using map.

(define subst-se
  (lambda (new old)
    (lambda (se)
      (if (symbol? se)
          (if (eq? se old) new se)
          (subst2 new old se)))))

(define subst2
  (lambda (new old se)
    (map (subst-se new old) se)))


(define partial-vector-sum
  (lambda (vec n)
        (if (zero? n)
            0
            (+ (vector-ref vec (- n 1))
               (partial-vector-sum vec (- n 1))))))


(define vector-sum
    (lambda (vec)
        (partial-vector-sum vec (vector-length vec))))               

;; 2.2.6
; 0 <= n <= len(vec)        

; we cannot deconstuct vector so we have to prove by "n".
; IH = if partial-vector-sum n returns sum of [0, (n-1)] elements of vector then it returns sum of [0,n] elements of vector too.
; let f be partial-vector-sum where vector is passed already.
; f (n) -> denotes (partial-vector-sum) called with vec and n.

; let vector be [x1,x2....... xn, xn+1,...]

; IH : f (n) = sum [0,n-1] -> f (n+1) = sum [0,n]

; f (0) -> 0. This can be proved trivially by looking at the definition of partial-vector-sum

; f (n+1) -> by construction we can see, if n != 0 then

; f(n+1) =  ele_at [(n+1) - 1)] + f [(n+1) - 1]
; f(n+1) =  ele_at [n] + f [n]
; f(n+1) = x(n+1) + sum [0,n-1] {from the IH}
; Hence, proved

