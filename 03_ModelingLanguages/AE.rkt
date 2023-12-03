#lang plai
; type definition for AE
(define-type AE
  [num (n number?)]
  [add (lhs AE?) (rhs AE?)]
  [sub (lhs AE?) (rhs AE?)])

; {+ {- 2 1} 3} => 4
(define ae1 (add (sub (num 2) (num 1)) (num 3)))
; ae1 is an instance of add instance, not sub instance
(sub? ae1)
; get the left-hand side of ae1 instance
(add-lhs ae1)
; get the right-hand side of ae1 instance
(add-rhs ae1)
; get the right-hand side of left-hand side of ae1 instance
(sub-rhs (add-lhs ae1))

;; [contract] parse: sexp -> AE
;; [purpose] to convert s-expressions into AEs
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(and (= 3 (length sexp))
          (eq? (first sexp) '+))
     (add (parse (second sexp))
          (parse (third sexp)))]
    [(and (= 3 (length sexp))
          (eq? (first sexp) '-))
     (sub (parse (second sexp))
          (parse (third sexp)))]
    [else (error 'parse "bad syntax: ~a" sexp)]
    ))

(test (parse '3) (num 3))
(test (parse '{+ 3 4}) (add (num 3) (num 4)))
(test (parse '{+ {- 3 4} 7}) (add (sub (num 3) (num 4)) (num 7)))
(test/exn (parse '{- 5 1 2}) "parse: bad syntax: (- 5 1 2)")

; [contract] interp: AE->number
(define (interp an-ae)
  (type-case AE an-ae
    [num (n) n]
    [add (l r) (+ (interp l) (interp r))]
    [sub (l r) (- (interp l) (interp r))]
    ))

(test (interp (parse '3)) 3)
(test (interp (parse '{+ 3 4})) 7)
(test (interp (parse '{+ {- 3 4} 7})) 6)
