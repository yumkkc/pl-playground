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

