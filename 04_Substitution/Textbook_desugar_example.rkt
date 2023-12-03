#lang plai
; Language 'ArithC' that supports number, addition and multiplication
(define-type ArithC
  [numC (n number?)]
  [plusC (l ArithC?) (r ArithC?)]
  [multC (l ArithC?) (r ArithC?)])

#|
; Parser for 'ArithC'
; [contract] parse : sexp -> 'ArithC'
(define (parse sexp)
  (match sexp
    [(? number?) (numC sexp)]
    [(list '+ l r) (plusC (parse l) (parse r))]
    [(list '* l r) (multC (parse l) (parse r))]
    [else (error 'parse "bad syntax:~a" sexp)]))
|#

; Interpreter for 'ArithC'
; [contract] interp : 'ArithC' -> number
(define (interp a)
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))

#|
(interp (plusC (numC 3) (numC 4)))
(interp (multC (numC 5) (numC 4)))
(interp (plusC (numC 6) (multC (numC 5) (numC 2))))
|#

#|
(test (interp (parse '{+ 3 4})) 7)
(test (interp (parse '{* 5 4})) 20)
(test (interp (parse '{+ 6 {* 5 2}})) 16)
|#

; Language 'ArithS' that supports number, addition, subtraction, and multiplication
(define-type ArithS
  [numS (n number?)]
  [plusS (l ArithS?) (r ArithS?)]
  [minusS (l ArithS?) (r ArithS?)]
  [multS (l ArithS?) (r ArithS?)])

; 'minusS' : syntactic sugar for subtraction

; ArithC represents our core language while subtraction represent our surface language. It's wise to record conceptually different ideas in distinct datatypes.

; Parser for 'ArithS'
; [contract] parse : sexp -> 'ArithS'
(define (parse sexp)
  (match sexp
    [(? number?) (numS sexp)]
    [(list '+ l r) (plusS (parse l) (parse r))]
    [(list '- l r) (minusS (parse l) (parse r))]
    [(list '* l r) (multS (parse l) (parse r))]
    [else (error 'parse "bad syntax:~a" sexp)]))

; Desugar function
; Desugaring allows us to simplify the surface language while still leveraging the capabilities of the core language.
; the subtraction operation is desugared into addition and multiplication by -1
; [contract] desugar : 'ArithS' -> 'ArithC'
(define (desugar as)
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [minusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]))

;(interp (desugar(minusS (numS 10) (numS 5))))

(test (interp (desugar (parse '{- 30 10}))) 20)
(test (interp (desugar (parse '{+ 6 {- 5 2}}))) 9)