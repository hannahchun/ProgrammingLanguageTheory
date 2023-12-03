#lang plai
; list -> number
; to get the length of a list
; (test (my-length '(a b c)) 3)
; (test (my-length empty) 0)

(define (my-length lst)
  (cond
    [(empty? lst) 0]
    [else (+ 1 (my-length (rest lst)))] ))

(test (my-length '(a b c)) 3)
(test (my-length empty) 0)
