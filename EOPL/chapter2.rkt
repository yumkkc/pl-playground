#lang scribble/doc

inductive definition of list is

<list-of-numbers> ::= ()
<list-of-numbers> ::= (<number> . <list-of-numbers>)


Exercise 2.1.1
--------------
(-7 . (3 . (14 . () )))

   <list-of-num>
=> (<number> . <list-of-numbers>)
=> (-7 . <list-of-numbers>)
=> (-7 . (<number> . <list-of-numbers>))
=> (-7 . (3 . <list-of-numbers>))
=> (-7 . (3 . (<nums> . <lon>)))
=> (-7 . (3 . (14 . (<lon>))))
=> (-7 . (3 . (14 . (() ))))

Exercise 2.1.2
--------------
<datum-repeat> ::= <datum> | <datum> <datum-repeat>
       <list>  ::= () | (<datum-repeat>)
<dotted-datum> ::= (<datum-repeat> . <datum>)
<vector>       ::= #() | #(<datum-repeat>)
<datum>        ::= <number> | <symbol> | <boolean> | <string>
                   | <list> | <dotted-datum> | <vector>


(#t (foo . ()) 3)

<list>
=> (<datum-repeat>) *
=> (<datum> <datum-repeat>) *
=> (<datum> <datum> <datum-repeat>) *
(<datum> <datum> <datum>) *
(<boolean> <datum> <datum>)
(#t <datum> <datum>)
(#t <dotted-datum> <datum>)
(#t (<datum-repeat> . <datum>) <datum>) *
(#t (<datum> . <datum>) <datum>) *
(#t (<symbol> . <datum>) <datum>) *
(#t (foo . <datum>) <datum>) *
(#t (foo . <list>) <datum>)
(#t (foo . ()) <datum>)
(#t (foo . ()) <number>)
(#t (foo . ()) 3)

"*" indicates the changes to the original. We needed more derivation
if we did not use kleen plus and star since we had to introduce <datum-repeat> which esentially does the same thing
as kleen-star, which is repeat but it does it in more steps.
