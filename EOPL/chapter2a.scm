; <s-list> ::= (<{symbol-expression}*>)
; <symbol-expression> := <symbol> | <s-list>

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
