#lang plai
; 4.10 Pairs and lists
; 4.10.1 Pair Constructors and Selectors
(pair? 1)
(pair? (cons 1 '()))
(pair? (cons 1 2))
(pair? (list 1 2))
(pair? '(1 2))
(pair? '())

(null? 1)
(null? '(1 2))
(null? '())
(null? (cdr (list 1)))

(cons 1 2)
(cons 1 '())

(car '(1 2))
(car (cons 2 3))

(cdr '(1 2))
(cdr '(1 2 3 4 5))
(cdr '(1))

null
'()
(eq? '() null)

(list? '(1 2))
(list? (cons 1'()))
(list? (cons 1 (cons 2'())))
(list? (cons 1 2))

(list 1 2 3 4)
(list (list 1 2) (list 3 4))
(list 'a 'b 'c)
(list '1 '2 '3)
(list empty (list 1 2 3))
null
empty

(list* 1 2)
(list* 1 2 (list 3 4))
(list? (list* 1 2))
(list? (list* 1 2 (list 3 4)))

(build-list 10 values)
(build-list 5 (lambda (x) (* x x)))

(length (list 1 2 3 4))
(length '())

(list-ref '(1 2 3) 0)
(list-ref (list 'a 'b 'c) 0)
(list-ref (list 'a 'b 'c) 1)
(list-ref (list 'a 'b 'c) 2)
(list-ref (cons 1 2) 0)
; (list-ref (cons 1 2) 1) returns an error


(cons 1 2)
(cons 1  (cons 2 empty))
; check the differences!
 
(list-ref (cons 1  (cons 2 empty)) 1)
(list-ref (cons 1 (cons 2 (cons 3 empty))) 1)
; does not return an error

(append (list 1 2) (list 3 4))
(append (list 1 2) (list 3 4) (list 5 6) (list 7 8))
(append (list 1 2) (list 3 4) (list 5 6) 7)
(list? (append (list 1 2) (list 3 4) (list 5 6) 7))

(first '(1 2 3 4 5 6 7 8 9 10))
(rest '(1 2 3 4 5 6 7 8 9 10))

; first vs car
(first '(1 2))
(car '(1 2))
;(first (cons 1 2)) produces error because (cons 1 2) is not a list
(car (cons 1 2))

; rest vs cdr
(rest '(1 2))
(cdr '(1 2))
;(rest (cons 1 2)) produces error because (cons 1 2) is not a list
(cdr (cons 1 2))

(map (lambda (number) (+ 1 number)) '(1 2 3 4))
(map (lambda (number1 number2) (+ number1 number2)) '(1 2 3 4) '(10 100 1000 10000))

(foldl cons '() '(1 2 3 4))
(foldl + 0 '(1 2 3 4))
(foldl (lambda (a b result) (* result (- a b))) 1 '(1 2 3) '(4 5 6))

(foldr cons '() '(1 2 3 4))
(foldr (lambda (v l) (cons (add1 v) l)) '() '(1 2 3 4))

(filter positive? '(1 -2 3 4 -5))

empty
(eq? empty null)

(cons? '(1 2))

(empty? '(1 2))
(empty? '())