#lang plai
; https://docs.racket-lang.org/guide/Pairs__Lists__and_Racket_Syntax.html

; When the second argument is not empty and not itself produced by cons, the result prints in a special way
; The two values joined with cons are printed between parentheses, but with a dot in between
(cons 1 2)
(cons "banana" "split")

; a value produced by cons is not alwarys a list but rather a pair
; the name rest also makes less sense for non-list pairs;
; the more traditional names for first and rest are car and cdr, respectively
(car (cons 1 2))
(cdr (cons 1 2))
(pair? empty)
(pair? (cons 1 2))
(pair? (list 1 2 3))
(cons (list 2 3) 1)
(cons 1 (list 2 3))