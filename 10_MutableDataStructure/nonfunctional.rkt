#lang plai
(define (f x)
  (+ x (read)))

(f 5)

(define g
  (local [(define b (box 0))]
    (lambda (x)
      (begin
        (set-box! b (+ x (unbox b)))
        (unbox b)))))

(g 5)

(define counter 0)
(define (h x)
  (begin
    (set! counter (+ x counter))
    counter))

(h 2)
