#lang plai
(define retry #f)

(+ (* 2 3) 10)
(+ (* (call/cc (lambda (k) (k 2))) 3) 10)
; (+ (* (let/cc k (k 2)) 3) 10)
(+ (* (call/cc (lambda (k) (set! retry k) 2)) 3) 10)
; (+ (* (let/cc k (set! retry k) 2) 3) 10)

(retry 3)
(retry 2)
(retry 1)
