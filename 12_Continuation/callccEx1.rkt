#lang plai
(define retry #f)

(define factorial
  (lambda (x)
    (if (= x 0)
        (call/cc (lambda (k) (set! retry k) 1))
        ; (let/cc k (set! retry k) 1)
        (* x (factorial (- x 1))))))

(factorial 4)
(retry 1)
(retry 2)

; call/cc
; entire context is saved as a continuation which is 'k'
; we assign this continuation to the global variable 'retry'

#|
(* 4 (factorial 3))
(* 4 (* 3 (factorial 2)))
(* 4 (* 3 (* 2 (factorial 1))))
(* 4 (* 3 (* 2 (* 1 (factorial 0)))))
(* 4 (* 3 (* 2 (* 1 1))))
24

(* 1 (factorial 0))
24

(* 2 (factorial 1))
(* 2 (* 1 (factorial 0)))
48
|#