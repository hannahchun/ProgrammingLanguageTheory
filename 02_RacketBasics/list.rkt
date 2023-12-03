#lang plai
; list
(list 'a 'b 'c)
; null is considered an empty list
null
; empty is considered an empty list
empty
(list empty (list 1 2 3))

; non-list pair
(cons 1 2)

(cons 1 empty)
(cons 'a (cons 2 empty))
(cons (cons 2 empty) 'a)

(list 1 2 3)
(cons 1 (cons 2 (cons 3 '())))
(list 1 2 3 empty)

(append (list 1 2) empty)
(append (list 1 2) (list 3 4))
(append (list 1 2) (list 'a 'b) (list true))

(first (list 1 2 3))
(rest  (list 1 2 3))
(first (rest (list 1 2)))

;'(...) creates a list. it distributes over elements
'(1 2 3)
'(a b)
'((a 2) (3 4))
'10

; distinguish empty / non-empty lists
(empty? empty)
(empty? (cons "head" empty))
(cons? empty)
(cons? (cons "head" empty))

(cons (list-ref (rest (list 1 2 3)) 0) empty)

(cons (list 1 2) (list 3 4))
(append (list 1 2) (list 3 4))
(list (list 1 2) 3 4) 
(cons 1 (list 2 3))
(cons (list 2 3) 1)
